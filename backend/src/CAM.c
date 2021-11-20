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

void eval_fst();
void eval_snd();
void eval_acc();
void eval_rest();
void eval_push();
void eval_swap();
void eval_loadi();
void eval_loadb();
void eval_clear();
void eval_cons();
void eval_cur();
void eval_pack();
void eval_skip();
void eval_stop();
void eval_app();
void eval_return();
void eval_call();
void eval_goto();
void eval_gotofalse();
void eval_switch();
void eval_abs();
void eval_neg();
void eval_not();
void eval_dec();
void eval_add_unsignedi();
void eval_mul_unsignedi();
void eval_min_unsignedi();
void eval_add_signedi();
void eval_mul_signedi();
void eval_min_signedi();
void eval_addf();
void eval_mulf();
void eval_minf();
void eval_gt_unsignedi();
void eval_lt_unsignedi();
void eval_ge_unsignedi();
void eval_le_unsignedi();
void eval_gt_signedi();
void eval_lt_signedi();
void eval_ge_signedi();
void eval_le_signedi();
void eval_gtf();
void eval_ltf();
void eval_gef();
void eval_lef();
void eval_eq_unsignedi();
void eval_eq_signedi();
void eval_eqf();
void eval_eq_bool();
/* Optimised instructions */
void eval_move();
void eval_pop ();
void eval_snoc();
void eval_comb();
void eval_gotoifalse();
void eval_switchi   ();
void eval_callrts   ();



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


  /* constant for each VMC */
  const uint8_t *code;
  UINT code_size;

  heap_t *heap;

  vmc_t *vmc;
} machine_state_t;

/* One of these per VMC later */
machine_state_t ms;

void cam_setup_machine_state(vmc_t *vmc) {
  ms.heap = vmc->heap;
  ms.code = vmc->code;
  ms.code_size=vmc->code_size;
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



static inline uint16_t get_label(){
  return (ms.code[ms.pc + 1] << 8) | ms.code[ms.pc + 2]; // merge 2 bytes
}

static inline uint16_t get_tag(){
  return (ms.code[ms.pc + 1] << 8) | ms.code[ms.pc + 2]; // merge 2 bytes
}

static bool is_all_contexts_stopped(){
  bool start = false;
  for(int i = 0; i < VMC_MAX_CONTEXTS; i++){
    start = start | ms.vmc->context_used[i];
  }
  return !start;
}

void eval_fst() {
  ms.pc++;
  cam_value_t v = heap_fst(ms.heap, (heap_index)ms.env.value);
  ms.env = v;
}

void eval_snd() {
  ms.pc--;
  cam_value_t v = heap_snd(&ms.heap, (heap_index)ms.env.value);
  ms.env = v;
}


void eval_acc() {
  uint8_t acc_n = ms.code[ms.pc + 1];

  for(unsigned int i = 0; i < acc_n; i++){
    cam_value_t v = heap_fst(&ms.heap, (heap_index)ms.env.value);
    ms.env = v;
  }
  cam_value_t v = heap_snd(&ms.heap, (heap_index)ms.env.value);
  ms.env = v;
  ms.pc += 2;
}

void eval_rest()  {
  uint8_t acc_n = ms.code[ms.pc+1];
  for(unsigned int i = 0; i < acc_n; i++){
    cam_value_t v = heap_fst(&vmc->heap, (heap_index)ms.env.value);
    ms.env = v;
  }
  ms.pc += 2;
}

void eval_push() {
  int i = spush(ms.env);
  if(i == 0){
    DEBUG_PRINT(("Stack push has failed"));
    ms.pc = -1;
    return;
  }
  ms.pc++;
}

void eval_swap() {

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

void eval_loadi() {
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

void eval_loadb() {
  INT bool_idx = (*pc_idx) + 1;
  uint8_t bool_val = ms.code[ms.pc + 1];
  cam_value_t v = { .value = (UINT)bool_val, .flags = 0};
  ms.env = v;
  ms.pc += 2;
}

void eval_clear() {
  cam_value_t empty_tuple = { .value = 0, .flags = 0 };
  ms.env = empty_tuple;
  ms.pc ++;
}

void eval_cons() {
  (*pc_idx)++;

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
    cam_value_t env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    ms.env = env_pointer;
    heap_set(&ms.heap, hi, hold_reg, e);
  }
  return;
}

void eval_cur() {
  uint16_t label = get_label();
  cam_value_t cam_label = { .value = (UINT)label, .flags = 0 };
  heap_index hi = vmc_heap_alloc_withGC(ms.vmc);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    ms.pc = -1;
  } else {
    cam_value_t env_pointer = { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    heap_set(&ms.heap, hi, ms.env, cam_label);
    ms.env = env_pointer;
    ms.pc += 3;
  }
  return;
}

void eval_pack() {
  uint16_t tag = get_tag();
  cam_value_t cam_tag =
    { .value = (UINT)tag, .flags = 0 };
  heap_index hi = vmc_heap_alloc_withGC(ms.vmc);
  if(hi == HEAP_NULL){
    DEBUG_PRINT(("Heap allocation has failed"));
    ms.pc = -1;
  } else {
    cam_value_t env_pointer = { .value = (UINT)hi, .flags = VALUE_PTR_BIT };
    heap_set(&vmc->heap, hi, cam_tag, ms.env);
    ms.env = env_pointer;
    ms.pc += 3;
  }
  return;
}

void eval_skip() {
  ms.pc++;
}

void eval_stop() {
  ms.vmc->context_used[vmc->current_running_context_id] = false;
  int i = dispatch(ms.vmc);
  if(i == -1)
    DEBUG_PRINT(("Ready Queue is empty\n"));
  if(is_all_contexts_stopped(ms.vmc)){
    ms.vmc->all_contexts_stopped = true;
  }
}

void eval_app() {

  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
    return;
  }

  heap_index closure_address = ms.env.value; // TODO: should we do a pointer check here?
                                             // closure or combinator, the if checks that

  cam_value_t heap_f = heap_fst(&ms.heap, closure_address);
  cam_value_t heap_s = heap_snd(&ms.heap, closure_address);

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
    heap_set(&ms.heap, hi, val, hold_reg);
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

void eval_return() {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
    return;
  }
  ms.pc = hold_reg.value;
}

void eval_call() {
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

void eval_goto() {
  uint16_t label = get_label();
  // GOTO doesn't store jump address on stack
  ms.pc = (INT)label;
}

void eval_gotofalse() {
  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;
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

void eval_switch() {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = spop(&hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    ms.pc = -1;
    return;
  }
  heap_index closure_address = ms.env.value; // TODO: should we do a pointer check here?
  cam_value_t tag_heap = heap_fst(&ms.heap, closure_address);
  cam_value_t val = heap_snd(&ms.heap, closure_address);
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
  heap_set(&ms.heap, hi, hold_reg, val);

  //goto label
  ms.pc = (INT)label_to_jump;
}

void eval_abs() {
  INT signed_i = (INT)ms.env.value;
  INT abs_i = abs(signed_i);
  cam_value_t v = { .value = (UINT)abs_i, .flags = 0};
  ms.env = v;
  ms.pc++;
}

void eval_neg() {
  UINT i = ms.env.value;
  INT j = -i; // XXX: might cause underflow for large uints
  cam_value_t v = { .value = (UINT)j, .flags = 0};
  ms.env = v;
  ms.pc++;

}

void eval_not() {
  UINT i = ms.env.value;
  UINT j = i ^ 1;
  cam_value_t v = { .value = j, .flags = 0};
  ms.env = v;
  ms.pc ++;

}

void eval_dec() {
  UINT i = ms.env.value;
  INT j = i - 1; // XXX: casting might cause issues for uint when outside int range
                 // dec should work with signed ints
  cam_value_t v = { .value = (UINT)j, .flags = 0};
  ms.env = v;
  ms.pc ++;
}

void eval_add_unsignedi() {
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

void eval_mul_unsignedi() {
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

void eval_min_unsignedi() {
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

void eval_add_signedi() {
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

void eval_mul_signedi() {

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

void eval_min_signedi() {
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


void eval_addf() {
  cam_register_t hold_reg = { .flags = 0, .value = 0 }; // init register
  int i = stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
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

void eval_mulf() {
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

void eval_minf() {
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

void eval_gt_unsignedi() {
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

void eval_lt_unsignedi() {
  cam_register_t hold_reg = { .flags = 0, .value = 0 };
  int i = stack_pop(&hold_reg);
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

void eval_ge_unsignedi() {
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

void eval_le_unsignedi() {
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

void eval_gt_signedi() {
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

void eval_lt_signedi() {
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

void eval_ge_signedi() {
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

void eval_le_signedi() {
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

void eval_gtf() {
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

void eval_ltf() {
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

void eval_gef() {
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

void eval_lef() {
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

void eval_eq_unsignedi() {
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


void eval_eq_signedi() {
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

void eval_eqf() {
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

void eval_eq_bool() {
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

void eval_move(){
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

void eval_pop (){
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

void eval_snoc(){
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
      heap_set(&vmc->heap, hi, ms.env, hold_reg);
      ms.env = env_pointer;
      ms.pc ++;
    }
  }
  return;
}
void eval_comb(){

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

    heap_set(&ms.heap, hi, cam_label, dummy_val);
    ms.env = env_pointer;
    ms.pc += 3;
  }
  return;
}
void eval_gotoifalse(){
  if ((e.value & 1) == 0){ // NOT SET; FALSE
    eval_goto();
  } else { // TRUE
   ms.pc += 3;
  }
  return;
}

void eval_switchi(){

  heap_index tag_val_pair = ms.env.value;
  cam_value_t tag_heap = heap_fst(&ms.heap, tag_val_pair);
  cam_value_t val      = heap_snd(&ms.heap, tag_val_pair);
  INT switch_size_idx = ms-pc + 1;
  uint8_t switch_size = ms.code[switch_size_idx];

  int label_to_jump = -1;
  for(uint32_t i = (switch_size_idx + 1); i <= (switch_size_idx + (switch_size * 4)); i+=4){
    uint16_t tag = (ms.code[i] << 8) | vmc->code_memory[i+1]; // merge 2 bytes
    uint16_t label = (ms-code[i+2] << 8) | ms-code[i+3]; // merge 2 bytes

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
/*  TODO ALL THE HANDLERS */


static int handle_spawn(vmc_t *vmc){

  /* IMP:
   * spawn starts a new process with the signature () -> ()
   * When we jump to this process (because of sync) the operation
   * is like processing an APP with the argument (). So the
   * argument () needs to be placed in the environment depending
   * on the cases of a closure [v:l] (snocced in this case) or
   * a combinator [l] (simply place () in the env in this case)
   */
  cam_value_t empty_tuple = { .value = 0, .flags = 0 };



  cam_register_t e = vmc->contexts[vmc->current_running_context_id].env;

  heap_index closure_address = e.value;


  cam_value_t heap_f = heap_fst(&vmc->heap, closure_address);
  cam_value_t heap_s = heap_snd(&vmc->heap, closure_address);

  if(heap_s.value == COMB){ // if combinator

    cam_value_t label = heap_f;

    vmc->contexts[vmc->current_running_context_id].env = empty_tuple;

    return spawn(vmc, (uint16_t)label.value); // will place PID in env


  } else { // not a combinator but a closure

    cam_value_t val = heap_f;
    cam_value_t label = heap_s;


    // Put (v, ()) of [v:l] on the env register; Read above why () comes;
    // spawn then copies the content of the env register to
    // the `env` register of the new context

    heap_index hi = vmc_heap_alloc_withGC(vmc);
    if(hi == HEAP_NULL){
      DEBUG_PRINT(("Heap allocation has failed"));
      return -1;
    }
    heap_set(&vmc->heap, hi, val, empty_tuple);
    cam_value_t new_env_pointer =
      { .value = (UINT)hi, .flags = VALUE_PTR_BIT };

    vmc->contexts[vmc->current_running_context_id].env = new_env_pointer;

    // Spawn will places the label graveyard address on the stack
    return spawn(vmc, (uint16_t)label.value); // will place PID in env

  }

}

static int handle_channel(vmc_t *vmc){
  UUID chan_id;
  int j = channel(vmc, &chan_id);
  if(j == -1){
    DEBUG_PRINT(("Error initializing a channel \n"));
    return j;
  }
  cam_value_t channel_cam = { .value = (UINT)chan_id, .flags = 0 };
  vmc->contexts[vmc->current_running_context_id].env = channel_cam;
  return 1;
}

static int handle_sendevt(vmc_t *vmc){
  cam_value_t message = vmc->contexts[vmc->current_running_context_id].env;

  cam_register_t hold_reg;
  int i =
    stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  UUID channel_id = (UUID)hold_reg.value;

  event_t send_evt;
  int j = sendEvt(vmc, &channel_id, message, &send_evt);
  if(j == -1){
    DEBUG_PRINT(("Error with sendEvt \n"));
    return j;
  }

  cam_value_t send_evt_env =
    { .value = (UINT)send_evt, .flags = VALUE_PTR_BIT };
  vmc->contexts[vmc->current_running_context_id].env = send_evt_env;
  return 1;
}

static int handle_recvevt(vmc_t *vmc){
  cam_value_t channel_cam = vmc->contexts[vmc->current_running_context_id].env;

  UUID channel_id = (UUID)channel_cam.value;

  event_t recv_evt;
  int j = recvEvt(vmc, &channel_id, &recv_evt);
  if(j == -1){
    DEBUG_PRINT(("Error with recvEvt \n"));
    return j;
  }

  cam_value_t recv_evt_env =
    { .value = (UINT)recv_evt, .flags = VALUE_PTR_BIT };
  vmc->contexts[vmc->current_running_context_id].env = recv_evt_env;
  return 1;
}

static int handle_sync(vmc_t *vmc){
  cam_value_t event_env = vmc->contexts[vmc->current_running_context_id].env;

  if(event_env.flags != VALUE_PTR_BIT){
    DEBUG_PRINT(("Thread number : %u", vmc->current_running_context_id));
    DEBUG_PRINT(("Pointer not found in the environment register \n"));
    return -1;
  }

  event_t evt = (event_t)event_env.value;

  int j = sync(vmc, &evt);
  if(j == -1){
    DEBUG_PRINT(("Error in synchronisation \n"));
    return j;
  }

  return 1;


}

static int handle_choose(vmc_t *vmc){
  cam_value_t e2 = vmc->contexts[vmc->current_running_context_id].env;

  cam_register_t e1;
  int i =
    stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &e1);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }

  event_t evt1 = (event_t)e1.value;
  event_t evt2 = (event_t)e2.value;

  event_t final_evt;

  choose(vmc, &evt1, &evt2, &final_evt);

  cam_value_t final_evt_cam =
    { .value = (UINT)final_evt, .flags = VALUE_PTR_BIT };

  vmc->contexts[vmc->current_running_context_id].env = final_evt_cam;

  return 1;

}

static int handle_spawnExternal(vmc_t *vmc){

  //spawnExternal : Channel a -> Int -> ()

  cam_value_t driver_details =
    vmc->contexts[vmc->current_running_context_id].env;

  cam_register_t hold_reg;
  int i =
    stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  UUID chan_id = (UUID)hold_reg.value;

  if(vmc->drivers[driver_details.value].is_synchronous){
    // synchronous driver like LEDs
    vmc->channels[chan_id].sync_driver_no = (UUID)driver_details.value;
  } else {
    // asynchronous drivers like buttons
    vmc->drivers[driver_details.value].channel_id = chan_id;
  }

  return 1;

}

static int handle_wrap(vmc_t *vmc){

  //wrap : Event a -> (a -> b) -> Event b

  cam_value_t wrapf_ptr =
    vmc->contexts[vmc->current_running_context_id].env;

  cam_register_t hold_reg;
  int i =
    stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }
  event_t current_evt = hold_reg.value;

  // IMP: we are guaranteed by the compiler tranformations that
  // current_evt is a base event; so traversal of event not needed

  // get pointer to cam_event_t
  cam_value_t cevt_ptr = heap_fst(&vmc->heap, (heap_index)current_evt);
  // get pointer to base_event_t
  cam_value_t bevt_ptr = heap_fst(&vmc->heap, (heap_index)cevt_ptr.value);
  // set the second of the cell that bevt_ptr is pointing to wrapf_ptr
  heap_set_snd(&vmc->heap, (heap_index)bevt_ptr.value, wrapf_ptr);

  //Place the modified event on the environment
  cam_value_t new_env = { .value = (UINT)current_evt, .flags = VALUE_PTR_BIT };
  vmc->contexts[vmc->current_running_context_id].env = new_env;

  return 1;

}

static int handle_time(vmc_t *vmc){

  //syncT : Time -> Time -> Event a -> a

  cam_value_t hold_reg1 = vmc->contexts[vmc->current_running_context_id].env;

  cam_register_t hold_reg2;
  int i =
    stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg2);
  if(i == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }

  cam_register_t hold_reg3;
  int j =
    stack_pop(&vmc->contexts[vmc->current_running_context_id].stack, &hold_reg3);
  if(j == 0){
    DEBUG_PRINT(("Stack pop has failed"));
    return -1;
  }

  Time baseline = (Time)hold_reg1.value;

  Time deadline = (Time)hold_reg2.value;

  // After calling the rts function `time` make sure the
  // env register points to `ev` so that we can `sync` next
  // because the sequence of bytecode will be - ..time; sync...
  vmc->contexts[vmc->current_running_context_id].env = hold_reg3;

  int k = time(vmc, baseline, deadline);
  if(k == -1){
    DEBUG_PRINT(("Error with syncT \n"));
    return k;
  }


  return 1;

  //FUTURE WORK
  //TODO: hold_reg1 and hold_reg2 should contain indices to the int pool
  // find index of 64 bit int baseline and deadline from the int pool

}

void eval_callrts(){
  uint8_t rts_op_no = ms.code_memory[ms.pc + 1];

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
