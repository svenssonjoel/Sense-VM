/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2020 Joel Svensson, Abhiroop Sarkar             				  */
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


#include <VMC.h>
#include <heap.h>
#include <CAM.h>
/* This is just an experiment and if we end up building on it, the
   range of numbers can be extended */
#if VMC_NUM_CONTAINERS >= 1 && VMC_NUM_CONTAINERS <= 2

vmc_t vm_containers[VMC_NUM_CONTAINERS];

#else
#error "VMC_NUM_CONTAINERS must be set to an integer value from 1 to 2"
#endif


#if VMC_NUM_CONTAINERS >= 1
uint8_t vmc_container_1_heap[VMC_CONTAINER_1_HEAP_SIZE_BYTES];
uint8_t vmc_container_1_stack[VMC_CONTAINER_1_STACK_SIZE_BYTES];
uint8_t vmc_container_1_arrays[VMC_CONTAINER_1_ARRAY_MEM_SIZE_BYTES];

const uint8_t vmc_container_1_code[] = {
  #include VMC_CONTAINER_1_BYTECODE_FILE
  ,0
};
#endif

#if VMC_NUM_CONTAINERS >= 2
uint8_t vmc_container_2_heap[VMC_CONTAINER_2_HEAP_SIZE_BYTES];
uint8_t vmc_container_2_stack[VMC_CONTAINER_2_STACK_SIZE_BYTES];
uint8_t vmc_container_2_arrays[VMC_CONTAINER_2_ARRAY_MEM_SIZE_BYTES];

const uint8_t vmc_container_2_code[] = {
  #include VMC_CONTAINER_2_BYTECODE_FILE
  ,0
};

#endif


int vmc_init(void) {

  int r = 0;
  int rl = 0;

  #if VMC_NUM_CONTAINERS >= 1
  rl = heap_init(&vm_containers[VMC_CONTAINER_1].heap, vmc_container_1_heap, VMC_CONTAINER_1_HEAP_SIZE_BYTES);
  if (!rl) return 0;
  vm_containers[VMC_CONTAINER_1].stack_memory   = vmc_container_1_stack;
  vm_containers[VMC_CONTAINER_1].code_memory    = vmc_container_1_code;
  vm_containers[VMC_CONTAINER_1].arrays_memory  = vmc_container_1_arrays;
  r++;
  #endif

  #if VMC_NUM_CONTAINERS >= 2
  rl = heap_init(&vm_containers[VMC_CONTAINER_2].heap, vmc_container_2_heap, VMC_CONTAINER_2_HEAP_SIZE_BYTES);
  if (!rl) return 0;
  vm_containers[VMC_CONTAINER_2].stack_memory  = vmc_container_2_stack;
  vm_containers[VMC_CONTAINER_2].code_memory   = vmc_container_2_code;
  vm_containers[VMC_CONTAINER_2].arrays_memory = vmc_container_2_arrays;
  r++;
  #endif

  return r;
}


int vmc_run(vmc_t *container) {

  for (int i = 0; i < VMC_MAX_CONTEXTS; i++) {
    container->context_used[i] = false;
  }

  INT pc = 0;
  /* Check valid code */
  uint32_t magic = 0;
  magic |= container->code_memory[pc++] << 24; /* not sure this shifting works out */
  magic |= container->code_memory[pc++] << 16;
  magic |= container->code_memory[pc++] << 8;
  magic |= container->code_memory[pc++];

  if (magic != 0xFEEDCAFE) return 0;

  /* uint8_t version = container->code_memory[pc++]; */
  pc++;

  uint16_t pool_size_ints;
  pool_size_ints = container->code_memory[pc++] << 8;
  pool_size_ints |= container->code_memory[pc++];

  pc += (pool_size_ints * 4);

  uint16_t pool_size_strings;
  pool_size_strings = container->code_memory[pc++] << 8;
  pool_size_strings |= container->code_memory[pc++];

  pc += pool_size_strings;

  uint16_t pool_size_native;
  pool_size_native = container->code_memory[pc++] << 8;
  pool_size_native |= container->code_memory[pc++];

  pc += (pool_size_native * 4);

  uint32_t code_size;
  code_size = container->code_memory[pc++] << 24;
  code_size |= container->code_memory[pc++] << 16;
  code_size |= container->code_memory[pc++] << 8;
  code_size |= container->code_memory[pc++];

  /* Now pc should be the index of the first instruction. */
  /* set up the parent context */
  /* Running all computations in parent context for now */
  cam_value_t v_empty = get_cam_val(0,0);
  container->context.env = v_empty;
  container->context.pc  = pc;

  cam_stack_t s = { .size = 0 };
  int w = stack_init(&s, container->stack_memory, 256);
  if (w == 0){
    DEBUG_PRINT(printf("Stack initialization has failed"));
    return -1; // indicates error
  }

  container->context.stack = s;

  /* Start executing instructions now */
  uint8_t current_inst = container->code_memory[pc];
  while(current_inst != 13){ // stop instruction
    (*evaluators[current_inst])(container, &pc);
    if(pc == -1){
      DEBUG_PRINT(printf("Instruction %u failed",current_inst));
      return -1; // error
    }
    current_inst = container->code_memory[pc];
  }
  /* Encountered STOP now */

  /* end */
  return 1;
}
