/*
    Copyright 2019 Joel Svensson	svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef _DRIVER_GPIO_
#define _DRIVER_GPIO_

#define DRIVER_GPIO_COMMAND_READ  0x0001
#define DRIVER_GPIO_COMMAND_SET   0x0002
#define DRIVER_GPIO_COMMAND_CLR   0x0004


extern bool init_gpio_driver(driver_rts_if_t *drv);


#endif
