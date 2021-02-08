

#include <zephyr.h>

#include <sys/ring_buffer.h>
#include <usb/usb_device.h>
#include <drivers/uart.h>
#include <drivers/gpio.h>

#include <stdio.h>

#define RING_BUF_SIZE 1024
uint8_t in_ring_buffer[RING_BUF_SIZE];
uint8_t out_ring_buffer[RING_BUF_SIZE];

struct ring_buf in_ringbuf;
struct ring_buf out_ringbuf;

static const struct device *usb_dev;

static volatile bool usb_cdc_enabled = false; 

static void interrupt_handler(const struct device *dev, void *user_data)
{
  (void)user_data;
  
  while (uart_irq_update(dev) && uart_irq_is_pending(dev)) {
    if (uart_irq_rx_ready(dev)) {
      int recv_len, rb_len;
      uint8_t buffer[64];
      size_t len = MIN(ring_buf_space_get(&in_ringbuf),
		       sizeof(buffer));

      recv_len = uart_fifo_read(dev, buffer, len);

      rb_len = ring_buf_put(&in_ringbuf, buffer, recv_len);
      if (rb_len < recv_len) {
	//silently dropping bytes
      }
    }

    if (uart_irq_tx_ready(dev)) {
      uint8_t buffer[64];
      int rb_len, send_len;

      rb_len = ring_buf_get(&out_ringbuf, buffer, sizeof(buffer));
      if (!rb_len) {
	uart_irq_tx_disable(dev);
	continue;
      }

      send_len = uart_fifo_fill(dev, buffer, rb_len);
      if (send_len < rb_len) {
	//LOG_ERR("Drop %d bytes", rb_len - send_len);
      }
    }
  }
}


void usb_printf(char *format, ...) {
  if (!usb_cdc_enabled) return;
  
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
      ring_buf_put(&out_ringbuf,
		   (print_buffer + num_written),
		   (len - num_written));
    irq_unlock(key);
    uart_irq_tx_enable(usb_dev);
  }
}



void usb_cdc_thread_main(void * a, void* b, void *c) {
  (void)a;
  (void)b;
  (void)c;


    uint32_t baudrate, dtr = 0U;
  int ret;

  usb_dev = device_get_binding("CDC_ACM_0");
  if (!usb_dev) {
    return;
  }

  ret = usb_enable(NULL);
  if (ret != 0) {
    return;
  }

  ring_buf_init(&in_ringbuf, sizeof(in_ring_buffer), in_ring_buffer);
  ring_buf_init(&out_ringbuf, sizeof(out_ring_buffer), out_ring_buffer);
  
  while (true) {
    uart_line_ctrl_get(usb_dev, UART_LINE_CTRL_DTR, &dtr);
    if (dtr) {
      break;
    } else {
      k_sleep(K_MSEC(100));
    }
  }
  
  ret = uart_line_ctrl_set(usb_dev, UART_LINE_CTRL_DCD, 1); 
  if (ret) { 
    //LOG_WRN("Failed to set DCD, ret code %d", ret); 
  } 
  ret = uart_line_ctrl_set(usb_dev, UART_LINE_CTRL_DSR, 1);
  if (ret) {
    //LOG_WRN("Failed to set DSR, ret code %d", ret);
  }

  k_busy_wait(1000000);
  
  ret = uart_line_ctrl_get(usb_dev, UART_LINE_CTRL_BAUD_RATE, &baudrate);
  if (ret) {
  } else {
  }

  uart_irq_callback_set(usb_dev, interrupt_handler);

  /* Enable rx interrupts */
  uart_irq_rx_enable(usb_dev);

  usb_cdc_enabled = true;
  
}
  
K_THREAD_STACK_DEFINE(usb_cdc_stack_area, 512);
struct k_thread usb_cdc_thread;


void start_usb_cdc_thread(void)  {

  k_tid_t usb_cdc_tid = k_thread_create(&usb_cdc_thread, usb_cdc_stack_area,
					K_THREAD_STACK_SIZEOF(usb_cdc_stack_area),
					usb_cdc_thread_main,
					NULL, NULL, NULL,
					5 /* prio */,
					0, K_NO_WAIT);

}