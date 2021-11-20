/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2021 Abhiroop Sarkar, Joel Svensson             		  */
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

#include <SVM_DEBUG.h>

#include <scheduler.h>
#include <heap.h>
#include <CAM.h>
#include <RTS.h>
#include <queue.h>

/*********************/
/* Scheduler tracing */

//#define TRACE_ON
#define MAX_TRACE_LENGTH 50

typedef struct scheduler_trace_s {
  UUID context_id;
  cam_register_t env;
  UINT pc;
  uint8_t instr;
  uint8_t  bytes[4];
  unsigned int sp;
  uint32_t num_msgs;
  uint32_t total_msgs;
  gc_stats_t gc_stats;
  struct scheduler_trace_s *next;
} scheduler_trace_t;

scheduler_trace_t trace_storage[MAX_TRACE_LENGTH];
int trace_next = 0;

scheduler_trace_t *trace = NULL;

void trace_add(UUID cid, cam_register_t env, UINT pc,
	       uint8_t instr, unsigned int sp, uint32_t num_msgs, uint32_t total_msgs,
	       uint8_t *bytes, gc_stats_t gc_stats) {

  if (trace_next == MAX_TRACE_LENGTH) {
    trace_next = 0;
  }

  scheduler_trace_t *curr = &trace_storage[trace_next];

  curr->context_id = cid;
  curr->env = env;
  curr->pc = pc;
  curr->instr = instr;
  curr->bytes[0] = bytes[0];
  curr->bytes[1] = bytes[1];
  curr->bytes[2] = bytes[2];
  curr->bytes[3] = bytes[3];
  curr->sp = sp;
  curr->next = trace;
  curr->num_msgs = num_msgs;
  curr->total_msgs = total_msgs;
  curr->gc_stats = gc_stats;
  trace = curr;
  trace_next++;
  trace_storage[trace_next].next = NULL;
}



void trace_print(void (*dbg_print)(const char *str, ...), int num) {

  scheduler_trace_t *curr = trace;

  int n = 0;

  while (curr) {
    dbg_print("*****************************************************\r\n");
    dbg_print("Trace position: %d \r\n", n++);
    dbg_print("Context: %d\r\n",curr->context_id );
    dbg_print("PC: %d\r\n", curr->pc);
    dbg_print("Environment: %u\r\n", curr->env.value);
    dbg_print("Instruction: 0x%x\r\n", curr->instr);
    dbg_print("Bytes: [0x%x , 0x%x, 0x%x, 0x%x]\r\n", curr->bytes[0], curr->bytes[1], curr->bytes[2], curr->bytes[3]);
    dbg_print("Msg num: %u\r\n", curr->num_msgs);
    dbg_print("Total num msgs %u\r\n", curr->total_msgs);
    dbg_print("GC Stats:\r\n");
    dbg_print("  Num mark phases: %llu\r\n", curr->gc_stats.num_mark_phases);
    dbg_print("  Num recovered: %llu\r\n", curr->gc_stats.num_recovered);
    dbg_print("  Num allocated: %llu\r\n", curr->gc_stats.num_allocated);
    curr = curr->next;
    if (n > num) break;
  }
}


static void initLogicalTime(vmc_t *vmc){
  Time currentTicks = sys_time_get_current_ticks();
  for(int i = 0; i < VMC_MAX_CONTEXTS; i ++){
    vmc->contexts[i].logicalTime = currentTicks;
  }
}



/*************/
/* Scheduler */
int scheduler(vmc_t *container,
	      message_read_poll_fun poll_msg,
	      message_read_block_fun block_msg,
	      message_queue_num_used_fun msgq_num_used) {

  //type: poll_msg(vmc_t *vmc, svm_msg_t *msg);
  //type: block_msg(vmc_t *vmc, svm_msg_t *msg);

  (void)poll_msg;

  svm_msg_t msg;

  dbg_print("Entered Scheduler\r\n");
  dbg_print("container address: %u\r\n", (uint32_t)container);

#ifdef TRACE_ON
  uint32_t total_msgs = 0;
#endif

  // set logical time
  initLogicalTime(container);


  while (true) {

    if(container->all_contexts_stopped){
      // All of the contexts have encountered the STOP operation; Program STOP
      break;
    }

    /* What if we want to pre-emt running context for an important
       message
       - threads cooperative.
       -
    */
    /* If we are doing nothing, block on the message queue */
    if (container->current_running_context_id == UUID_NONE) {

      block_msg(container, &msg);

      /*handle msg */
      int msg_r = handle_msg(container, &msg);
      if (msg_r  <= 0) {
	dbg_print("Error in handle_msg: %d\r\n",msg_r);
	return -1;
	/* continue as if nothing has happend.
	   This should be like throwing the message away */
      }
    } else {
      if(cam_step() == -1){
        dbg_print("Instruction failed");
        return -1; // error
      }
    }
  }
  /* end */
  dbg_print("Closing down scheduler\r\n");
  return 1;
}
