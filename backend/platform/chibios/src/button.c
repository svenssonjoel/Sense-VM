/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2021 Joel Svensson, Abhiroop Sarkar 				  */
/* 										  */
/* Permission is hereby granted, free of charge, to any person obtaining a copy	  */
/* of this software and associated documentation files (the "Software"), to deal  */
/* in the Software without restriction, including without limitation the rights	  */
/* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell	  */
/* copies of the Software, and to permit persons to whom the Software is	  */
/* furnished to do so, subject to the following conditions:			  */
/* 										  */
/* The above copyright notice and this permission notice shall be included in all */
/* copies or substantial portions of the Software.				  */
/* 										  */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR	  */
/* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,	  */
/* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE	  */
/* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER	  */
/* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  */
/* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  */
/* SOFTWARE.									  */
/**********************************************************************************/

#include <button.h>
#include <sys/sys_time.h>

#include <hal_pal.h>
#include <platform.h>

#define BUTTON_THREAD_STACK_SIZE 4096
#define NUM_BUTTON_MESSAGES 4

static THD_WORKING_AREA(button_debouncer_wa, BUTTON_THREAD_STACK_SIZE);
static thread_t *button_debouncer;

static mailbox_t button_mail;
static msg_t button_msgs[NUM_BUTTON_MESSAGES];

//static memory_pool_t *button_msg_pool;
static button_driver_internal_t button_msg_repr[NUM_BUTTON_MESSAGES] __attribute__((aligned((4))));
static bool button_thread_initialized = false;

static MEMORYPOOL_DECL(button_msg_pool, sizeof(button_driver_internal_t), PORT_NATURAL_ALIGN, NULL);


/* Debounce buttons and detect (to ignore button spamming) */

#define BUTTON_BOUNCING_0    0
#define BUTTON_STABLE_0      1
#define BUTTON_BOUNCING_1    2
#define BUTTON_STABLE_1      3
#define BUTTON_SPAMMING      4
#define BUTTON_POLL_MAX      10
#define BUTTON_POLL_INTERVAL 10

bool cb_called = false;
int  msgs_sent = 0;

static THD_FUNCTION(button_debouncer_thread, arg) {
  (void) arg;
  msg_t msg_value; /* pointer into memory pool */
  button_driver_internal_t msg_data;

  dbg_print("waiting for button mail\n");
  /* sleep until there is mail */

  while (1) {

    dbg_print("hello %d\n", msgs_sent);

    if (cb_called) {
      dbg_print("button event!\n");
      cb_called = false;
    }

    if (msgs_sent > 0) {
      int r = chMBFetchTimeout(&button_mail, &msg_value, TIME_INFINITE);
      if (r == MSG_OK) {
	msg_data = *(button_driver_internal_t*)msg_value;
	chPoolFree(&button_msg_pool, (void*)msg_value);      
	dbg_print("buttonID: %d\n", msg_data.id);
	dbg_print("buttonState: %d\n", msg_data.state);

	dbg_print("buttonPort: %u\n", msg_data.port);
	dbg_print("buttonPad:  %u\n", msg_data.pad);
	dbg_print("buttonMode: %u\n", msg_data.mode);

	palEnablePadEvent(msg_data.port, msg_data.pad, msg_data.mode);
	
	/* re-enable interrupts on the button */
	msgs_sent --;
	dbg_print("message received\n");
      } else {
	dbg_print("no message\n");
      }
    }
    
    chThdSleepMilliseconds(100);

  }
  
  int r = chMBFetchTimeout(&button_mail, &msg_value, TIME_INFINITE);

  if (r == MSG_OK) {
    
    msg_data = *(button_driver_internal_t*)msg_value;
    chPoolFree(&button_msg_pool, (void*)msg_value);

    int state;
    if (msg_data.state == false) {
      state = BUTTON_BOUNCING_0;
    } else {
      state = BUTTON_BOUNCING_1;
    }

    uint32_t button_state;
    bool done = false;
    for (int i = 0; i < BUTTON_POLL_MAX; i ++) {
      button_state = palReadPad(msg_data.port, msg_data.pad);
      switch(state) {
      case BUTTON_BOUNCING_0:
	if (button_state == 1) {
	  state = BUTTON_BOUNCING_1;
	}
	else {
	  state = BUTTON_STABLE_0;
	}
	break;
      case BUTTON_BOUNCING_1:
	if (button_state == 0) {
	  state = BUTTON_BOUNCING_0;
	}
	else {
	  state = BUTTON_STABLE_1;
	}
	break;
      case BUTTON_STABLE_0:
	if (button_state == 0) {
	  done = true;
	} else {
	  state = BUTTON_BOUNCING_0;
	}
	break;
      case BUTTON_STABLE_1:
	if (button_state == 1) {
	  done = true;
	} else {
	  state = BUTTON_BOUNCING_1;
	}
	break;
      default:
	break;
      }

      if (done || (state == BUTTON_STABLE_1) || (state == BUTTON_STABLE_0)) {
	/* svm_msg_t msg;  */
	/* msg.sender_id = msg_data.id;  */
	/* msg.timestamp = 0; // TODO: implement */
	/* msg.data = button_state;  // 1 or 0 */
	
	/* if (msg_data.interop->send_message(msg_data.interop, msg) == -1) { */
	/*   /\* Message was not send due to queue being full.  */
	/*      What do we do in this case?  *\/  */
	/* } */
	break;
      }
      chThdSleepMilliseconds(BUTTON_POLL_INTERVAL);
    }
    /* re-enable interrupts on the button */
    palEnablePadEvent(msg_data.port, msg_data.pad, msg_data.mode);
  }
  /* if the message is not OK, there is not much to do.  */ 
}


void button_cb(void *arg) {
  cb_called = true;
  chSysLockFromISR();
  button_driver_internal_t *button = (button_driver_internal_t *)arg;
  palDisablePadEventI(button->port, button->pad);
  bool state = palReadPad(button->port, button->pad);
  button->state = state;
  
  button_driver_internal_t *m = (button_driver_internal_t*)chPoolAllocI(&button_msg_pool);
  if (m) {
    *m = *button; /* make a copy */
    
    msg_t r = chMBPostI(&button_mail, (uint32_t)m);
    if (r != MSG_OK) {
      chPoolFreeI(&button_msg_pool, (void*)m);
    } else if (r == MSG_OK){
      msgs_sent ++;
    }
  }

  chSysUnlockFromISR();
  /* svm_msg_t msg; */
  /* msg.sender_id = button->id; */
  /* msg.timestamp = 0; // TODO: implement */
  /* msg.data = state;  // 1 or 0 */

  
  /* if (button->interop->send_message(button->interop, msg) == -1) { */
  /*   /\* Message was not send due to queue being full. */
  /*      What do we do in this case?  *\/ */
  /* } */

  
}

#include <ll/ll_button.h>

static uint32_t ll_button_control(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  (void) this;
  (void) data;
  (void) data_size;
  return 0;
}

static uint32_t ll_button_data_available(struct ll_driver_s *this) {
  (void) this;
  return 1;
}

static uint32_t ll_button_data_writeable(struct ll_driver_s *this) {
  (void) this;
  return 0;
}

static uint32_t ll_button_read(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  ll_button_driver_t *b = (ll_button_driver_t*)this->driver_info;

  uint32_t r = 0;
  
  if (data_size == 4) {
    data[0] = b->internal.state;
    data[1] = b->internal.state >> 8;
    data[2] = b->internal.state >> 16;
    data[3] = b->internal.state >> 24;
    r = 4;
  }
  
  return r;
}

static uint32_t ll_button_write(struct ll_driver_s *this, uint8_t *data, uint32_t data_size) {
  (void) this;
  (void) data;
  (void) data_size;
  return 0;
}

bool ll_button_init(ll_driver_t* lld, ll_button_driver_t *bdrv) {

  lld->driver_info = bdrv;
 
  lld->is_synchronous = false;
  lld->ll_control_fun = ll_button_control;
  lld->ll_read_fun = ll_button_read;
  lld->ll_write_fun = ll_button_write;
  lld->ll_data_readable_fun = ll_button_data_available;
  lld->ll_data_writeable_fun = ll_button_data_writeable;




  dbg_print("buttonPort: %u\n", bdrv->internal.port);
  dbg_print("buttonPad:  %u\n", bdrv->internal.pad);
  dbg_print("buttonMode: %u\n", bdrv->internal.mode);

  if (!button_thread_initialized) {
    dbg_print("Initializing button\n");
    chPoolLoadArray(&button_msg_pool, button_msg_repr, NUM_BUTTON_MESSAGES);
    chMBObjectInit(&button_mail, button_msgs, NUM_BUTTON_MESSAGES);

    button_debouncer = chThdCreateStatic(button_debouncer_wa,
    					 sizeof (button_debouncer_wa),
    					 NORMALPRIO - 21,
    					 button_debouncer_thread,
    					 NULL);
    button_thread_initialized = true;
  }
  return true;
}
