/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2020 Abhiroop Sarkar                            		  */
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

#ifdef DEBUG
#include <stdio.h>
# define DEBUG_PRINT(x) printf x
#else
# define DEBUG_PRINT(x) do {} while (0)
#endif

#include <event.h>

bool poll_sendq(chan_send_queue_t *q){
  while(true){
    send_data_t send_data;
    int op_status = chan_send_q_front(q, &send_data);
    if(op_status == -1){ //empty queue
      return false;
    } else {
      if(*send_data.dirty_flag){
        send_data_t temp;
        chan_send_q_dequeue(q, &temp); // no need to check status we know there is data
      } else {
        return true; // the actual dequeing should happen inside doFn
      }
    }
  }
}

bool poll_recvq(chan_recv_queue_t *q){
  while(true){
    recv_data_t recv_data;
    int op_status = chan_recv_q_front(q, &recv_data);
    if(op_status == -1){ //empty queue
      return false;
    } else {
      if(*recv_data.dirty_flag){
        recv_data_t temp;
        chan_recv_q_dequeue(q, &temp); // no need to check status
                                       // we know there is data
      } else {
        return true; // the actual dequeing should happen inside doFn
      }
    }
  }
}
