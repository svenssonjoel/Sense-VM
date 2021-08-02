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

#ifndef __TYPEDEFS_H_
#define __TYPEDEFS_H_

#include <stdint.h>
#include <flags.h>

typedef uint32_t UINT;
typedef int32_t  INT;
typedef uint8_t  UUID;
typedef uint64_t Time;

#define UUID_NONE 0xFF
#define DRIVER_NULL 0xFF

typedef struct {
  value_flags_t flags;
  UINT          value;
} cam_value_t;


typedef struct {
  uint32_t sender_id;     // Index into an array of drivers maintained by "low-level"
  uint32_t msg_type;      // Encode what kind of message ? (this could be 2 bytes, if driver_id is also 2 bytes)
  uint32_t data;          // Data payload, driver specific message or pointer  
  Time timestamp;
} svm_msg_t;

#define SENDER_ID_SYS_TIME 0xFFFFFFFF

#endif
