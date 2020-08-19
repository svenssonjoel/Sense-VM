/**********************************************************************************/
/* MIT License									  */
/* 										  */
/* Copyright (c) 2020 Joel Svensson             				  */
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

#include <heap.h>
#include <platform.h>

#include <stdlib.h> // later remove this include when no memory "malloced" in this file. 

/************/
/* Globals  */
/************/


heap_index free_list = HEAP_NULL;



/************************************/
/* Heap Creation and Initialization */
/************************************/

/*@ requires n_cells > 0 && n_cells < N_MAX_HEAP_CELLS ; */ 
heap_cell_t* heap_init(unsigned int n_cells) {

  heap_cell_t* heap = malloc(sizeof(heap_cell_t) * n_cells);

  if (heap) {
    for (unsigned int i = 0; i < n_cells; i ++) {
      heap[i].flags = HEAP_FLAGS_DEFAULT;
    }
  }

  return heap;
}







/**********************/
/* Garbage Collection */
/**********************/



