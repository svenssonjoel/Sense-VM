
all:
	west build -p auto -b nrf52840dk_nrf52840 .

discovery:
	west build -p auto -b stm32f4_disco .  -DDTC_OVERLAY_FILE="stm32f4_discovery.overlay"

dev_kit:
	west build -p auto -b nrf52840dk_nrf52840 .

nrf_mod:
	west build -p auto -b nrf_module_v1 -- -DBOARD_ROOT=../ . -DDTC_OVERLAY_FILE="nrf52_module.overlay"

clean:
	rm -r build/
