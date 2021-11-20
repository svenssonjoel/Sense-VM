/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2020, 2021 Joel Svensson, Abhiroop Sarkar 				  */
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

#include <CAM.h>
#include <VMC.h>
#include <stdlib.h>
#include <string.h>

#include <ll/ll_driver.h>

/* Combinators */

#define COMB 4294967295


/* Each eval function is called with the vmc state and the
 * current index of the program counter (pointed at the opcode).
 * The eval function internally increments the pc_idx. The
 * caller simply checks if pc_idx ever returns a negative value,
 * which contains semantic error info.
 */

/*Jump convention:
 * When making a jump, calculate the jump address + arguments
 * and store that on the stack. A return simply pops off that
 * address which is the address of the next opcode and jumps
 */

void eval_fst(void);
void eval_snd(void);
void eval_acc(void);
void eval_rest(void);
void eval_push(void);
void eval_swap(void);
void eval_loadi(void);
void eval_loadb(void);
void eval_clear(void);
void eval_cons(void);
void eval_cur(void);
void eval_pack(void);
void eval_skip(void);
void eval_stop(void);
void eval_app(void);
void eval_return(void);
void eval_call(void);
void eval_goto(void);
void eval_gotofalse(void);
void eval_switch(void);
void eval_abs(void);
void eval_neg(void);
void eval_not(void);
void eval_dec(void);
void eval_add_unsignedi(void);
void eval_mul_unsignedi(void);
void eval_min_unsignedi(void);
void eval_add_signedi(void);
void eval_mul_signedi(void);
void eval_min_signedi(void);
void eval_addf(void);
void eval_mulf(void);
void eval_minf(void);
void eval_gt_unsignedi(void);
void eval_lt_unsignedi(void);
void eval_ge_unsignedi(void);
void eval_le_unsignedi(void);
void eval_gt_signedi(void);
void eval_lt_signedi(void);
void eval_ge_signedi(void);
void eval_le_signedi(void);
void eval_gtf(void);
void eval_ltf(void);
void eval_gef(void);
void eval_lef(void);
void eval_eq_unsignedi(void);
void eval_eq_signedi(void);
void eval_eqf(void);
void eval_eq_bool(void);
/* Optimised instructions */
void eval_move(void);
void eval_pop (void);
void eval_snoc(void);
void eval_comb(void);
void eval_gotoifalse(void);
void eval_switchi   (void);
void eval_callrts   (void);



eval_fun evaluators[] =
  { eval_fst,
    eval_snd,
    eval_acc,
    eval_rest,
    eval_push,
    eval_swap,
    eval_loadi,
    eval_loadb,
    eval_clear,
    eval_cons,
    eval_cur,
    eval_pack,
    eval_skip,
    eval_stop,
    eval_app,
    eval_return,
    eval_call,
    eval_goto,
    eval_gotofalse,
    eval_switch,
    eval_abs,
    eval_neg,
    eval_not,
    eval_dec,
    eval_add_signedi,
    eval_mul_signedi,
    eval_min_signedi,
    eval_addf,
    eval_mulf,
    eval_minf,
    eval_gt_signedi,
    eval_lt_signedi,
    eval_eq_signedi,
    eval_ge_signedi,
    eval_le_signedi,
    eval_add_unsignedi,
    eval_mul_unsignedi,
    eval_min_unsignedi,
    eval_gt_unsignedi,
    eval_lt_unsignedi,
    eval_eq_unsignedi,
    eval_ge_unsignedi,
    eval_le_unsignedi,
    eval_gtf,
    eval_ltf,
    eval_eqf,
    eval_gef,
    eval_lef,
    eval_eq_bool,
    eval_move,
    eval_pop,
    eval_snoc,
    eval_comb,
    eval_gotoifalse,
    eval_switchi,
    eval_callrts  // 0x37 : 55
  };


/*******************************/
/* Machine registers and state */

typedef struct {
  /* Changes per context */
  UINT pc;
  cam_register_t env;
  UINT sp;


  UINT *stack_data;
  value_flags_t *stack_flags;
  UINT stack_size;

  UINT instruction_quota; // Instruction evaluation quota
                          // TODO: Set each time scheduler
                          // runs "step" 
  
  /* Constant for each VMC */
  const uint8_t *code;
  UINT code_size;

  heap_t *heap;

  vmc_t *vmc;
} machine_state_t;

/* One of these per VMC later */
machine_state_t ms;


/*********************************/
/* MACHINE CONTROL FUNCTIONALITY */


void cam_setup_machine_state(vmc_t *vmc) {
  ms.heap = &vmc->heap;
  ms.code = vmc->code_memory;
  ms.code_size=vmc->code_size;
}


/* execute a byte code operation */
/* TODO: Try to see if we can run some predetermined number of steps */
/* for each call to cam_step. This should improve efficiency a lot!  */
/* One idea could be to execute 10 instructions or until there is  a */
/* blocking instruction. Whichever comes first.                      */
/* Potentially the evaluator could return a boolean that indicates   */
/* if it can keep going or not. Or maybe better, the evaluator       */
/* returns a number symbolising how many more instructions the loop  */
/* should process. The a blocking operation can return 0 and all     */
/* other instructions returns a count.                               */
int cam_step(void) {
  uint8_t inst = ms.code[ms.pc];
  evaluators[inst]();
  return ms.pc; 
}
/* Error handling for cam_step: thought.      */
/* Add some inflooping code to each program   */
/*                                            */
/* label_fatal: skip                          */
/*              goto label_fatal              */
/*                                            */
/* and in case of some unrecoverable error    */
/* set pc to label_fatal.                     */
/* could have different labels for different  */
/* kinds of errors. out of memory for example */


/* A context switch will now be somewhat more expensive       */
/* But if we run some number of instructions between switches */
/* this should quickly be amortized.
                          */
int cam_context_switch(UUID ctx) {
  
  /* copy the running context values into VMC context structures */
  ms.vmc->contexts[ms.vmc->current_running_context_id].env = ms.env;
  ms.vmc->contexts[ms.vmc->current_running_context_id].pc = ms.pc;
  ms.vmc->contexts[ms.vmc->current_running_context_id].stack.sp = ms.sp;
  
  /* copy the target context values into machine state */
  ms.vmc->current_running_context_id = ctx;
  ms.env = ms.vmc->contexts[ctx].env;
  ms.pc  = ms.vmc->contexts[ctx].pc;
  ms.sp  = ms.vmc->contexts[ctx].stack.sp;

  ms.stack_data  = ms.vmc->contexts[ctx].stack.data;
  ms.stack_flags = ms.vmc->contexts[ctx].stack.flags;
  ms.stack_size  = ms.vmc->contexts[ctx].stack.size;

  return 1; /* TODO: error checking */
}



/********************************/
/* Stack manipulation functions */
int spush(cam_value_t cvalue) {
  if (ms.sp == ms.stack_size) {
    DEBUG_PRINT(("Stack Overflow\n"));
    return 0;
  }
  ms.stack_data[ms.sp] = cvalue.value;
  ms.stack_flags[ms.sp++] = cvalue.flags;
  return 1;
}

int spop(cam_register_t *r) {
  if (ms.sp == 0) {
    DEBUG_PRINT(("Stack Underflow\n"));
    return 0;
  }
  ms.sp--;
  r->value = ms.stack_data[ms.sp];
  r->flags = ms.stack_flags[ms.sp];
  return 1;
}



static inline uint16_t get_label(void){
  return (ms.code[ms.pc + 1] << 8) | ms.code[ms.pc + 2]; // merge 2 bytes
}

static inline uint16_t get_tag(void){
  return (ms.code[ms.pc + 1] << 8) | ms.code[ms.pc + 2]; // merge 2 bytes
}

static bool is_all_contexts_stopped(void){
  bool start = false;
  for(int i = 0; i < VMC_MAX_CONTEXTS; i++){
    start = start | ms.vmc->context_used[i];
  }
  return !start;
}

void eval_fst(void) {
  ms.pc++;
  cam_value_t v = heap_fst(ms.heap, (heap_index)ms.env.value);
  ms.env = v;
}

void eval_snd(void) {
  ms.pc--;
  cam_value_t v = heap_snd(ms.heap, (heap_index)ms.env.value);
  ms.env = v;
}


void eval_acc(void) {
  uint8_t acc_n = ms.code[ms.pc + 1];

  for(unsigned int i = 0; i < acc_n; i++){
    cam_value_t v = heap_fst(ms.heap, (heap_index)ms.env.value);
    ms.env = v;
  }
  cam_value_t v = heap_snd(ms.heap, (heap_index)ms.env.value);
  ms.env = v;
  ms.pc += 2;
}

void eval_rest(void)  {
  uint8_t acc_n = ms.code[ms.pc+1];
  for(unsigned int i = 0; i < acc_n; i++){
    cam_value_t v = heap_fst(ms.heap, (heap_index)ms.env.value);
    ms.env = v;
  }
  ms.pc += 2;
}

void eval_push(void) {
  int i = spush(ms.env);
  if(i == 0){
    DEBUG_PRINT(("Stack push has failed"));
    ms.pc = -1;
    return;
  }
  ms.pc++;
}

void eval_swap(void) {

  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
    return;
  }
  int j = spush(ms.env);
  if(j == 0){
    DEBUG_PRINT(("Stack push has failed"));
    ms.pc = -1;
    return;
  }
  ms.env = hold_reg;
  ms.pc ++;
}

void eval_loadi(void) {
  uint16_t int_idx = (ms.code[ms.pc + 1] << 8) | ms.code[ms.pc + 2];
  INT int_pool_offset = 7; //TODO: Should we verify the int pool size here?
  INT i_idx = int_pool_offset + 4 * int_idx; // each int 4 bytes wide
  uint8_t byte0 = ms.code[i_idx];
  uint8_t byte1 = ms.code[i_idx + 1];
  uint8_t byte2 = ms.code[i_idx + 2];
  uint8_t byte3 = ms.code[i_idx + 3];
  INT i = (byte0 << 24) | (byte1 << 16) | (byte2 << 8) | byte3;
  cam_value_t v = { .value = (UINT)i, .flags = 0};
  ms.env = v;
  ms.pc += 3;
}

void eval_loadb(void) {
  uint8_t bool_val = ms.code[ms.pc + 1];
  cam_value_t v = { .value = (UINT)bool_val, .flags = 0};
  ms.env = v;
  ms.pc += 2;
}

void eval_clear(void) {
  cam_value_t empty_tuple = { .value = 0, .flags = 0 };
  ms.env = empty_tuple;
  ms.pc ++;
}

void eval_cons(void) {

  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
    return;
  }
  heap_index hi = vmc_heap_alloc_withGC(ms.vmc);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    ms.pc = -1;
  } else {
    // We have room for a tuple
    cam_value_t env_pointer = { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    heap_set(ms.heap, hi, hold_reg, ms.env);
    ms.env = env_pointer;
    ms.pc ++;
  }
  return;
}

void eval_cur(void) {
  uint16_t label = get_label();
  cam_value_t cam_label = { .value = (UINT)label, .flags = 0 };
  heap_index hi = vmc_heap_alloc_withGC(ms.vmc);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    ms.pc = -1;
  } else {
    cam_value_t env_pointer = { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    heap_set(ms.heap, hi, ms.env, cam_label);
    ms.env = env_pointer;
    ms.pc += 3;
  }
  return;
}

void eval_pack(void) {
  uint16_t tag = get_tag();
  cam_value_t cam_tag =
    { .value = (UINT)tag, .flags = 0 };
  heap_index hi = vmc_heap_alloc_withGC(ms.vmc);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    ms.pc = -1;
  } else {
    cam_value_t env_pointer = { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    heap_set(ms.heap, hi, cam_tag, ms.env);
    ms.env = env_pointer;
    ms.pc += 3;
  }
  return;
}

void eval_skip(void) {
  ms.pc++;
}

void eval_stop(void) {
  ms.vmc->context_used[ms.vmc->current_running_context_id] = false;
  int i = dispatch(ms.vmc);
  if(i == -1)
    DEBUG_PRINT(("Ready Queue is empty\n"));
  if(is_all_contexts_stopped()){
    ms.vmc->all_contexts_stopped = true;
  }
}

void eval_app(void) {

  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
    return;
  }

  heap_index closure_address = ms.env.value; // TODO: should we do a pointer check here?
                                             // closure or combinator, the if checks that

  cam_value_t heap_f = heap_fst(ms.heap, closure_address);
  cam_value_t heap_s = heap_snd(ms.heap, closure_address);

  if(heap_s.value == COMB){ // if combinator

    cam_value_t label = heap_f;

    ms.env = hold_reg;


    //jump to label
    INT jump_address = ms.pc + 1; // see Jump convention at the top
    cam_value_t j_add = { .value = (UINT)jump_address };
    int j = spush(j_add);
    if(j == 0){
      DEBUG_PRINT(("Stack push has failed"));
      ms.pc = -1;
      return;
    }
    ms.pc = (INT)label.value;

  } else { // not a combinator but a closure

    cam_value_t val = heap_f;
    cam_value_t label = heap_s;

    heap_index hi = vmc_heap_alloc_withGC(ms.vmc);
    if(hi == HEAP_NULL){
      DEBUG_PRINT(("Heap allocation has failed"));
      ms.pc = -1;
      return;
    }
    heap_set(ms.heap, hi, val, hold_reg);
    cam_value_t new_env_pointer = { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    ms.env = new_env_pointer;


    //jump to label
    INT jump_address = ms.pc + 1; // see Jump convention at the top
    cam_value_t j_add = { .value = (UINT)jump_address };
    int j = spush(j_add);
    if(j == 0){
      DEBUG_PRINT(("Stack push has failed"));
      ms.pc = -1;
      return;
    }
    ms.pc = (INT)label.value;
  }
}

void eval_return(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
    return;
  }
  ms.pc = hold_reg.value;
}

void eval_call(void) {
  uint16_t label = get_label();
  INT jump_address = ms.pc + 3; // see Jump convention at the top
  cam_value_t j_add = { .value = (UINT)jump_address };
  int i = spush(j_add);
  if(i == 0){
    DEBUG_PRINT(("Stack push has failed"));
    ms.pc = -1;
    return;
  }
  ms.pc = (INT)label;
}

void eval_goto(void) {
  uint16_t label = get_label();
  // GOTO doesn't store jump address on stack
  ms.pc = (INT)label;
}

void eval_gotofalse(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
    return;
  }
  if ((ms.env.value & 1) == 0){ // NOT SET; FALSE
    ms.env = hold_reg;
    eval_goto();
  } else { // TRUE
    ms.env = hold_reg;
    ms.pc += 3;
  }
}

void eval_switch(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
    return;
  }
  heap_index closure_address = ms.env.value; // TODO: should we do a pointer check here?
  cam_value_t tag_heap = heap_fst(ms.heap, closure_address);
  cam_value_t val = heap_snd(ms.heap, closure_address);
  INT switch_size_idx = ms.pc + 1;
  uint8_t switch_size = ms.code[switch_size_idx];

  int label_to_jump = -1;

  for(uint32_t i = (switch_size_idx + 1); i <= (switch_size_idx + (switch_size * 4)); i+=4){
    uint16_t tag = (ms.code[i] << 8) | ms.code[i+1]; // merge 2 bytes
    uint16_t label = (ms.code[i+2] << 8) | ms.code[i+3]; // merge 2 bytes

    if(tag_heap.value == (UINT)tag ||
       (UINT)tag == 65535){ //wildcard check; wildcard tag = max(uint16_t) = 65535
      label_to_jump = label;
      break;
    }
  }
  if(label_to_jump == -1){
    DEBUG_PRINT(("Tag %u not found while switching", tag_heap.value));
    ms.pc = -1;
    return;
  }

  heap_index hi = vmc_heap_alloc_withGC(ms.vmc);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    ms.pc = -1;
    return;
  }
  cam_value_t env_pointer = { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
  ms.env = env_pointer;
  heap_set(ms.heap, hi, hold_reg, val);

  //goto label
  ms.pc = (INT)label_to_jump;
}

void eval_abs(void) {
  INT signed_i = (INT)ms.env.value;
  INT abs_i = abs(signed_i);
  cam_value_t v = { .value = (UINT)abs_i, .flags = 0};
  ms.env = v;
  ms.pc++;
}

void eval_neg(void) {
  UINT i = ms.env.value;
  INT j = -i; // XXX: might cause underflow for large uints
  cam_value_t v = { .value = (UINT)j, .flags = 0};
  ms.env = v;
  ms.pc++;

}

void eval_not(void) {
  UINT i = ms.env.value;
  UINT j = i ^ 1;
  cam_value_t v = { .value = j, .flags = 0};
  ms.env = v;
  ms.pc ++;

}

void eval_dec(void) {
  UINT i = ms.env.value;
  INT j = i - 1; // XXX: casting might cause issues for uint when outside int range
                 // dec should work with signed ints
  cam_value_t v = { .value = (UINT)j, .flags = 0};
  ms.env = v;
  ms.pc ++;
}

void eval_add_unsignedi(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    cam_register_t final_value = { .flags = 0, .value = hold_reg.value + ms.env.value };
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_mul_unsignedi(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    cam_register_t final_value = { .flags = 0, .value = hold_reg.value * ms.env.value };
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_min_unsignedi(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    cam_register_t final_value = { .flags = 0, .value = hold_reg.value - ms.env.value };
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_add_signedi(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    /* I think this may be overkill */
    INT temp1;
    INT temp2;
    INT temp3;
    cam_register_t final_value = { .flags = 0, .value = 0 };
    memcpy(&temp1, &hold_reg.value, sizeof(UINT));
    memcpy(&temp2, &ms.env.value, sizeof(UINT));
    temp3 = temp1 + temp2;
    memcpy(&final_value.value, &temp3, sizeof(INT));
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_mul_signedi(void) {

  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    INT temp1;
    INT temp2;
    INT temp3;
    cam_register_t final_value = { .flags = 0, .value = 0 };
    memcpy(&temp1, &hold_reg.value, sizeof(UINT));
    memcpy(&temp2, &ms.env.value, sizeof(UINT));
    temp3 = temp1 * temp2;
    memcpy(&final_value.value, &temp3, sizeof(INT));
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_min_signedi(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    INT temp1;
    INT temp2;
    INT temp3;
    cam_register_t final_value = { .flags = 0, .value = 0 };
    memcpy(&temp1, &hold_reg.value, sizeof(UINT));
    memcpy(&temp2, &ms.env.value, sizeof(UINT));
    temp3 = temp1 - temp2;
    memcpy(&final_value.value, &temp3, sizeof(INT));
    ms.env = final_value;
    ms.pc ++;
  }
}


void eval_addf(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    /* Here, this may actually be needed (not overkill) */
    float temp1;
    float temp2;
    float temp3;
    cam_register_t final_value = { .flags = 0, .value = 0 };
    memcpy(&temp1, &hold_reg.value, sizeof(UINT));
    memcpy(&temp2, &ms.env.value, sizeof(UINT));
    temp3 = temp1 + temp2;
    memcpy(&final_value.value, &temp3, sizeof(float));
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_mulf(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    float temp1;
    float temp2;
    float temp3;
    cam_register_t final_value = { .flags = 0, .value = 0 };
    memcpy(&temp1, &hold_reg.value, sizeof(UINT));
    memcpy(&temp2, &ms.env.value, sizeof(UINT));
    temp3 = temp1 * temp2;
    memcpy(&final_value.value, &temp3, sizeof(float));
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_minf(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    float temp1;
    float temp2;
    float temp3;
    cam_register_t final_value = { .flags = 0, .value = 0 };
    memcpy(&temp1, &hold_reg.value, sizeof(UINT));
    memcpy(&temp2, &ms.env.value, sizeof(UINT));
    temp3 = temp1 - temp2;
    memcpy(&final_value.value, &temp3, sizeof(float));
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_gt_unsignedi(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 };
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    cam_register_t final_value = { .flags = 0, .value = hold_reg.value > ms.env.value };
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_lt_unsignedi(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 };
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    cam_register_t final_value = { .flags = 0, .value = hold_reg.value < ms.env.value };
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_ge_unsignedi(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 };
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    cam_register_t final_value = { .flags = 0, .value = hold_reg.value >= ms.env.value };
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_le_unsignedi(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 };
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    cam_register_t final_value = { .flags = 0, .value = hold_reg.value <= ms.env.value };
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_gt_signedi(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    INT temp1;
    INT temp2;
    INT temp3;
    cam_register_t final_value = { .flags = 0, .value = 0 };
    memcpy(&temp1, &hold_reg.value, sizeof(UINT));
    memcpy(&temp2, &ms.env.value, sizeof(UINT));
    temp3 = temp1 > temp2;
    memcpy(&final_value.value, &temp3, sizeof(INT));
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_lt_signedi(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    INT temp1;
    INT temp2;
    INT temp3;
    cam_register_t final_value = { .flags = 0, .value = 0 };
    memcpy(&temp1, &hold_reg.value, sizeof(UINT));
    memcpy(&temp2, &ms.env.value, sizeof(UINT));
    temp3 = temp1 < temp2;
    memcpy(&final_value.value, &temp3, sizeof(INT));
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_ge_signedi(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    INT temp1;
    INT temp2;
    INT temp3;
    cam_register_t final_value = { .flags = 0, .value = 0 };
    memcpy(&temp1, &hold_reg.value, sizeof(UINT));
    memcpy(&temp2, &ms.env.value, sizeof(UINT));
    temp3 = temp1 >= temp2;
    memcpy(&final_value.value, &temp3, sizeof(INT));
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_le_signedi(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    INT temp1;
    INT temp2;
    INT temp3;
    cam_register_t final_value = { .flags = 0, .value = 0 };
    memcpy(&temp1, &hold_reg.value, sizeof(UINT));
    memcpy(&temp2, &ms.env.value, sizeof(UINT));
    temp3 = temp1 <= temp2;
    memcpy(&final_value.value, &temp3, sizeof(INT));
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_gtf(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    float temp1;
    float temp2;
    float temp3;
    cam_register_t final_value = { .flags = 0, .value = 0 };
    memcpy(&temp1, &hold_reg.value, sizeof(UINT));
    memcpy(&temp2, &ms.env.value, sizeof(UINT));
    temp3 = temp1 > temp2;
    memcpy(&final_value.value, &temp3, sizeof(float));
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_ltf(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    float temp1;
    float temp2;
    float temp3;
    cam_register_t final_value = { .flags = 0, .value = 0 };
    memcpy(&temp1, &hold_reg.value, sizeof(UINT));
    memcpy(&temp2, &ms.env.value, sizeof(UINT));
    temp3 = temp1 < temp2;
    memcpy(&final_value.value, &temp3, sizeof(float));
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_gef(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    float temp1;
    float temp2;
    float temp3;
    cam_register_t final_value = { .flags = 0, .value = 0 };
    memcpy(&temp1, &hold_reg.value, sizeof(UINT));
    memcpy(&temp2, &ms.env.value, sizeof(UINT));
    temp3 = temp1 >= temp2;
    memcpy(&final_value.value, &temp3, sizeof(float));
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_lef(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    float temp1;
    float temp2;
    float temp3;
    cam_register_t final_value = { .flags = 0, .value = 0 };
    memcpy(&temp1, &hold_reg.value, sizeof(UINT));
    memcpy(&temp2, &ms.env.value, sizeof(UINT));
    temp3 = temp1 <= temp2;
    memcpy(&final_value.value, &temp3, sizeof(float));
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}


// Equality on base types

void eval_eq_unsignedi(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    cam_register_t final_value = { .flags = 0, .value = hold_reg.value == ms.env.value };
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}


void eval_eq_signedi(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    INT temp1;
    INT temp2;
    INT temp3;
    cam_register_t final_value = { .flags = 0, .value = 0 };
    memcpy(&temp1, &hold_reg.value, sizeof(UINT));
    memcpy(&temp2, &ms.env.value, sizeof(UINT));
    temp3 = temp1 == temp2;
    memcpy(&final_value.value, &temp3, sizeof(INT));
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_eqf(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    float temp1;
    float temp2;
    float temp3;
    cam_register_t final_value = { .flags = 0, .value = 0 };
    memcpy(&temp1, &hold_reg.value, sizeof(UINT));
    memcpy(&temp2, &ms.env.value, sizeof(UINT));
    temp3 = temp1 == temp2;
    memcpy(&final_value.value, &temp3, sizeof(float));
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_eq_bool(void) {
  cam_register_t hold_reg = { .flags = 0, .value = 0 };
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    cam_register_t final_value = { .flags = 0, .value = hold_reg.value == ms.env.value };
    ms.env = final_value;
    ms.pc ++;
  }
  return;
}

void eval_move(void){
  int i = spush(ms.env);
  if(i == 0){
    DEBUG_PRINT(("Stack push has failed"));
    ms.pc = -1;
  } else {
    cam_value_t empty_tuple = { .value = 0, .flags = 0 };
    ms.env = empty_tuple;
    ms.pc ++;
  }
  return;
}

void eval_pop (void){
  cam_register_t r;
  int i = spop(&r);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    ms.env = r;
    ms.pc ++;
  }
  return;
}

void eval_snoc(void){
  cam_register_t hold_reg;
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
  } else {
    heap_index hi = vmc_heap_alloc_withGC(ms.vmc);
    if(hi == HEAP_NULL){
      DEBUG_PRINT(("Heap allocation has failed"));
      ms.pc = -1;
    } else {
      // Assuming we have space for atleast one tuple
      // Do we check this as well?
      cam_value_t env_pointer = { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
      heap_set(ms.heap, hi, ms.env, hold_reg);
      ms.env = env_pointer;
      ms.pc ++;
    }
  }
  return;
}
void eval_comb(void){

  uint16_t label = get_label();
  cam_value_t cam_label = { .value = (UINT)label, .flags = 0 };
  heap_index hi = vmc_heap_alloc_withGC(ms.vmc);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    ms.pc = -1;
  } else {
    cam_value_t env_pointer = { .value = (UINT)hi, .flags = VALUE_PTR_BIT };

    // This value is used to demarcate a heap cell as
    // storing a combinator value rather than a closure
    cam_value_t dummy_val = { .value = COMB };

    heap_set(ms.heap, hi, cam_label, dummy_val);
    ms.env = env_pointer;
    ms.pc += 3;
  }
  return;
}
void eval_gotoifalse(void){
  if ((ms.env.value & 1) == 0){ // NOT SET; FALSE
    eval_goto();
  } else { // TRUE
   ms.pc += 3;
  }
  return;
}

void eval_switchi(void){

  heap_index tag_val_pair = ms.env.value;
  cam_value_t tag_heap = heap_fst(ms.heap, tag_val_pair);
  cam_value_t val      = heap_snd(ms.heap, tag_val_pair);
  INT switch_size_idx = ms.pc + 1;
  uint8_t switch_size = ms.code[switch_size_idx];

  int label_to_jump = -1;
  for(uint32_t i = (switch_size_idx + 1); i <= (switch_size_idx + (switch_size * 4)); i+=4){
    uint16_t tag = (ms.code[i] << 8) | ms.code[i+1]; // merge 2 bytes
    uint16_t label = (ms.code[i+2] << 8) | ms.code[i+3]; // merge 2 bytes

    if(tag_heap.value == (UINT)tag ||
       (UINT)tag == 65535){ //wildcard check; wildcard tag = max(uint16_t) = 65535
      label_to_jump = label;
      break;
    }
  }
  if(label_to_jump == -1){
    DEBUG_PRINT(("Tag %u not found while switching", tag_heap.value));
    ms.pc = -1;
    return;
  }
  ms.env = val;
  //goto label
  ms.pc = (INT)label_to_jump;
}

/**************************************************************/
/*       RTS HANDLERS                                         */


static int handle_spawn(void){

  /* IMP:
   * spawn starts a new process with the signature () -> ()
   * When we jump to this process (because of sync) the operation
   * is like processing an APP with the argument (). So the
   * argument () needs to be placed in the environment depending
   * on the cases of a closure [v:l] (snocced in this case) or
   * a combinator [l] (simply place () in the env in this case)
   */
  int r = 0;
  cam_value_t empty_tuple = { .value = 0, .flags = 0 };

  cam_register_t e = ms.env;

  heap_index closure_address = e.value;


  cam_value_t heap_f = heap_fst(ms.heap, closure_address);
  cam_value_t heap_s = heap_snd(ms.heap, closure_address);

  if(heap_s.value == COMB){ // if combinator

    cam_value_t label = heap_f;

    ms.env = empty_tuple;

    r =  spawn(ms.vmc, (uint16_t)label.value); // will place PID in env


  } else { // not a combinator but a closure

    cam_value_t val = heap_f;
    cam_value_t label = heap_s;

    // Put (v, ()) of [v:l] on the env register; Read above why () comes;
    // spawn then copies the content of the env register to
    // the `env` register of the new context

    heap_index hi = vmc_heap_alloc_withGC(ms.vmc);
    if(hi == HEAP_NULL){
      DEBUG_PRINT(("Heap allocation has failed"));
      r = -1;
    } else {
      heap_set(ms.heap, hi, val, empty_tuple);
      cam_value_t new_env_pointer = { .value = (UINT)hi, .flags = VALUE_PTR_BIT };

      ms.env = new_env_pointer;

      // Spawn will places the label graveyard address on the stack
      r = spawn(ms.vmc, (uint16_t)label.value); // will place PID in env
    }
  }
  return r;
}

static int handle_channel(void){
  UUID chan_id;
  int r = -1;
  int j = channel(ms.vmc, &chan_id);
  if(j == -1){
    DEBUG_PRINT(("Error initializing a channel \n"));
  } else {
    cam_value_t channel_cam = { .value = (UINT)chan_id, .flags = 0 };
    ms.env = channel_cam;
    r = 1;
  }
  return r;
}

static int handle_sendevt(void){
  cam_value_t message = ms.env;

  cam_register_t hold_reg;
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  UUID channel_id = (UUID)hold_reg.value;

  event_t send_evt;
  int j = sendEvt(ms.vmc, &channel_id, message, &send_evt);
  if(j == -1){
    DEBUG_PRINT(("Error with sendEvt \n"));
    return j;
  }

  cam_value_t send_evt_env =
    { .value = (UINT)send_evt, .flags = VALUE_PTR_BIT };
  ms.env = send_evt_env;
  return 1;
}

static int handle_recvevt(void){
  cam_value_t channel_cam = ms.env;

  UUID channel_id = (UUID)channel_cam.value;

  event_t recv_evt;
  int j = recvEvt(ms.vmc, &channel_id, &recv_evt);
  if(j == -1){
    DEBUG_PRINT(("Error with recvEvt \n"));
    return j;
  }

  cam_value_t recv_evt_env =
    { .value = (UINT)recv_evt, .flags = VALUE_PTR_BIT };
  ms.env = recv_evt_env;
  return 1;
}

static int handle_sync(void){
  cam_value_t event_env = ms.env;

  if(event_env.flags != VALUE_PTR_BIT){
    DEBUG_PRINT(("Thread number : %u", ms.vmc->current_running_context_id));
    DEBUG_PRINT(("Pointer not found in the environment register \n"));
    return -1;
  }

  event_t evt = (event_t)event_env.value;

  int j = sync(ms.vmc, &evt);
  if(j == -1){
    DEBUG_PRINT(("Error in synchronisation \n"));
    return j;
  }

  return 1;
}

static int handle_choose(void){
  cam_value_t e2 = ms.env;

  cam_register_t e1;
  int i = spop(&e1);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }

  event_t evt1 = (event_t)e1.value;
  event_t evt2 = (event_t)e2.value;

  event_t final_evt;

  choose(ms.vmc, &evt1, &evt2, &final_evt);

  cam_value_t final_evt_cam =
    { .value = (UINT)final_evt, .flags = VALUE_PTR_BIT };

  ms.env = final_evt_cam;
  return 1;
}

static int handle_spawnExternal(void){

  //spawnExternal : Channel a -> Int -> ()

  cam_value_t driver_details = ms.env;

  cam_register_t hold_reg;
  int i =
    spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  UUID chan_id = (UUID)hold_reg.value;

  if(ms.vmc->drivers[driver_details.value].is_synchronous){
    // synchronous driver like LEDs
    ms.vmc->channels[chan_id].sync_driver_no = (UUID)driver_details.value;
  } else {
    // asynchronous drivers like buttons
    ms.vmc->drivers[driver_details.value].channel_id = chan_id;
  }
  return 1;
}

static int handle_wrap(void){

  //wrap : Event a -> (a -> b) -> Event b

  cam_value_t wrapf_ptr = ms.env;

  cam_register_t hold_reg;
  int i =
    spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  event_t current_evt = hold_reg.value;

  // IMP: we are guaranteed by the compiler tranformations that
  // current_evt is a base event; so traversal of event not needed

  // get pointer to cam_event_t
  cam_value_t cevt_ptr = heap_fst(ms.heap, (heap_index)current_evt);
  // get pointer to base_event_t
  cam_value_t bevt_ptr = heap_fst(ms.heap, (heap_index)cevt_ptr.value);
  // set the second of the cell that bevt_ptr is pointing to wrapf_ptr
  heap_set_snd(ms.heap, (heap_index)bevt_ptr.value, wrapf_ptr);

  //Place the modified event on the environment
  cam_value_t new_env = { .value = (UINT)current_evt, .flags = VALUE_PTR_BIT };
  ms.env = new_env;

  return 1;
}

static int handle_time(void){

  //syncT : Time -> Time -> Event a -> a

  cam_value_t hold_reg1 = ms.env;

  cam_register_t hold_reg2;
  int i = spop(&hold_reg2);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }

  cam_register_t hold_reg3;
  int j = spop(&hold_reg3);
  if(j == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }

  /* How does this work? The cam values are just 32 bit */
  Time baseline = (Time)hold_reg1.value;

  Time deadline = (Time)hold_reg2.value;

  // After calling the rts function `time` make sure the
  // env register points to `ev` so that we can `sync` next
  // because the sequence of bytecode will be - ..time; sync...
  ms.env = hold_reg3;

  int k = time(ms.vmc, baseline, deadline);
  if(k == -1){
    DEBUG_PRINT(("Error with syncT \n"));
    return k;
  }
  return 1;

  //FUTURE WORK
  //TODO: hold_reg1 and hold_reg2 should contain indices to the int pool
  // find index of 64 bit int baseline and deadline from the int pool
}

void eval_callrts(void) {
  uint8_t rts_op_no = ms.code[ms.pc + 1];

  // do all operations here
    /* spawn     - 0 */
    /* channel   - 1 */
    /* sendEvt   - 2 */
    /* recvEvt   - 3 */
    /* sync      - 4 */
    /* choose    - 5 */
    /* spawnExternal - 6 */
    /* wrap      - 7 */
    /* time      - 8 */

  int ret_code = -1;
  switch(rts_op_no){
    case 0:
      ret_code = handle_spawn();
      break;
    case 1:
      ret_code = handle_channel();
      break;
    case 2:
      ret_code = handle_sendevt();
      break;
    case 3:
      ret_code = handle_recvevt();
      break;
    case 4:
      ret_code = handle_sync();
      break;
    case 5:
      ret_code = handle_choose();
      break;
    case 6:
      ret_code = handle_spawnExternal();
      break;
    case 7:
      ret_code = handle_wrap();
      break;
    case 8:
      ret_code = handle_time();
      break;
    default:
      DEBUG_PRINT(("Invalid RTS op number"));
      ms.pc = ret_code;
      return;
  }
  if(ret_code == -1){
    DEBUG_PRINT(("Error in RTS function"));
    ms.pc = ret_code;
  } else {

    ms.pc += 2;
  }
}
