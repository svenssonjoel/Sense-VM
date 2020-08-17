#define true 1
#define false 0
#define MAX_SIZE 1024
#include <stdint.h>

typedef int bool;

typedef struct {
   uint32_t security;
   // other details
	
}heap_elem_t;

typedef union {
   int32_t i;
   float f;
   bool b;
   heap_elem_t* e;
} stack_elem_t;

typedef enum { INT, FLOAT, BOOL, HEAP_P } elem_t;

typedef struct { 
    int top; 
    unsigned capacity;
    elem_t stack_type_indicator; 
    stack_elem_t* array; 
}stack; 

int main(){
  return -1;
}
