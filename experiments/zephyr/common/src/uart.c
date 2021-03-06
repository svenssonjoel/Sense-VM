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

#include <uart.h>
#include <stdio.h>

/* TODO: Surely there are problems with this
         if multiple os-threads read and write
	 using the same uart device (uart_dev_t) */



/* ************************* */
/* Interrupt service routine */

static void uart_isr(const struct device *dev, void *args)
{
  uart_dev_t *bufs = (uart_dev_t*)args;

  while (uart_irq_update(dev) && uart_irq_is_pending(dev)) {
    if (uart_irq_rx_ready(dev)) {
      int recv_len, rb_len;
      uint8_t buffer[64];
      size_t len = MIN(ring_buf_space_get(&bufs->in_ringbuf),
		       sizeof(buffer));

      recv_len = uart_fifo_read(dev, buffer, len);

      rb_len = ring_buf_put(&bufs->in_ringbuf, buffer, recv_len);
      if (rb_len < recv_len) {
	//silently dropping bytes
      }
    }

    if (uart_irq_tx_ready(dev)) {
      uint8_t c;
      int rb_len, send_len;

      rb_len = ring_buf_get(&bufs->out_ringbuf, &c, 1);
      if (!rb_len) {
	uart_irq_tx_disable(dev);
	continue;
      }

      send_len = uart_fifo_fill(dev, &c, 1);
    }
  }
}

/* ******************************** */
/* Initialization and configuration */

bool uart_init(uart_if_t uif, uart_dev_t *u,
	       uint8_t *in_buffer,
	       uint32_t in_size,
	       uint8_t *out_buffer,
	       uint32_t out_size) {

  switch(uif) {
  case UART0:
    u->dev = device_get_binding("UART_0");
    break;
  case UART1:
    u->dev = device_get_binding("UART_1");
    break;
  case UART2:
    u->dev = device_get_binding("UART_2");
    break;
  case UART3:
    u->dev = device_get_binding("UART_3");
    break;
  case UART4:
    u->dev = device_get_binding("UART_4");
    break;
  case UART5:
    u->dev = device_get_binding("UART_5");
    break;
  case UART6:
    u->dev = device_get_binding("UART_6");
    break;
  case UART7:
    u->dev = device_get_binding("UART_7");
    break;
  }

  if (u->dev) {

    ring_buf_init(&u->in_ringbuf, in_size, in_buffer);
    ring_buf_init(&u->out_ringbuf, out_size, out_buffer);


    uart_irq_callback_user_data_set(u->dev, uart_isr, (void*)u);
    uart_irq_rx_enable(u->dev);
  }
  return (bool)u->dev;
}

bool uart_get_baudrate(uart_dev_t *u, uint32_t *baud) {
  return (bool)uart_line_ctrl_get(u->dev, UART_LINE_CTRL_BAUD_RATE, baud);
}

/* ************************************* */
/* Are there bytes ?                     */

bool uart_data_available(uart_dev_t *dev) {
  return !ring_buf_is_empty(&dev->out_ringbuf);
}

int uart_ndata_available(uart_dev_t *dev) {
  return
    ring_buf_capacity_get(&dev->in_ringbuf) -
    ring_buf_space_get(&dev->in_ringbuf);
}



/* ************************************* */
/* Printing to and reading from the UART */

int uart_get_char(uart_dev_t *buffs) {

  int n;
  uint8_t c;
  unsigned int key = irq_lock(); /* disables all interrupts */
                                 /* TODO: Maybe it is possible to specifically just turn 
				    of the uart interrupt in question? */
  n = ring_buf_get(&buffs->in_ringbuf, &c, 1);
  irq_unlock(key);
  if (n == 1) {
    return c;
  }
  return -1;
}

int uart_read_bytes(uart_dev_t *dev, uint8_t *data, uint32_t data_size) {

  if (data_size > (ring_buf_capacity_get(&dev->in_ringbuf) -
		   ring_buf_space_get(&dev->in_ringbuf))) return 0;

  int n;
  unsigned int key = irq_lock();
  n = ring_buf_get(&dev->in_ringbuf, data, data_size);
  irq_unlock(key);
  return n;
}

int uart_put_char(uart_dev_t *buffs, char c) {

  int n = 0;
  unsigned int key = irq_lock();
  n = ring_buf_put(&buffs->out_ringbuf, &c, 1);
  irq_unlock(key);
  uart_irq_tx_enable(buffs->dev);
  return n;
}

int uart_write_bytes(uart_dev_t *dev, uint8_t *data, uint32_t data_size) {

  uint32_t free_space = ring_buf_space_get(&dev->out_ringbuf);
  if (free_space < data_size) return 0;
  unsigned int key = irq_lock();
  int n = ring_buf_put(&dev->out_ringbuf, data, data_size);
  irq_unlock(key);
  uart_irq_tx_enable(dev->dev);
  return n;
}

void uart_printf(uart_dev_t *buffs, char *format, ...) {

  va_list arg;
  va_start(arg, format);
  int len;
  static char print_buffer[4096];

  len = vsnprintf(print_buffer, 4096,format, arg);
  va_end(arg);

  int num_written = 0;
  while (len - num_written > 0) {
    unsigned int key = irq_lock();
    num_written +=
      ring_buf_put(&buffs->out_ringbuf,
		   (print_buffer + num_written),
		   (len - num_written));
    irq_unlock(key);
    uart_irq_tx_enable(buffs->dev);
  }

}
