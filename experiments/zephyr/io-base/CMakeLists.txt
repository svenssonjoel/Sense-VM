# SPDX-License-Identifier: Apache-2.0

cmake_minimum_required(VERSION 3.13.1)

find_package(Zephyr HINTS $ENV{ZEPHYR_BASE})
project(peripheral_hr)

FILE(GLOB app_sources src/*.c)
FILE(GLOB extra_sources ../common/src/abstraction/zephyr/usb_cdc.c)

target_sources(app PRIVATE ${app_sources} ${extra_sources})
target_include_directories(app PRIVATE ../common/src/abstraction/zephyr )

zephyr_library_include_directories(${ZEPHYR_BASE}/samples/bluetooth)
