# Don't need network
HOST_NETWORK_OPTIONS := --nonet
DISABLE_NETWORK := 1

COMPONENT_DEPENDS := \
	SmingTest \
	FatIFS \
	SdStorage \
	LittleFS

HWCONFIG := fatifs-test

# Time in milliseconds to pause after a test group has completed
CONFIG_VARS += TEST_GROUP_INTERVAL
TEST_GROUP_INTERVAL ?= 500
APP_CFLAGS += -DTEST_GROUP_INTERVAL=$(TEST_GROUP_INTERVAL)

# Time in milliseconds to wait before re-starting all tests
# Set to 0 to perform a system restart after all tests have completed
CONFIG_VARS += RESTART_DELAY
ifndef RESTART_DELAY
ifeq ($(SMING_ARCH),Host)
RESTART_DELAY = 0
else
RESTART_DELAY ?= -1
endif
endif
APP_CFLAGS += -DRESTART_DELAY=$(RESTART_DELAY)

.PHONY: execute
execute:
	$(Q) $(MAKE) run-test ENABLE_STORAGE_SIZE64=0 ENABLE_FILE_SIZE64=0 ENABLE_EXFAT=0
	$(Q) $(MAKE) run-test ENABLE_STORAGE_SIZE64=1 ENABLE_FILE_SIZE64=0 ENABLE_EXFAT=0
	$(Q) $(MAKE) run-test ENABLE_STORAGE_SIZE64=1 ENABLE_FILE_SIZE64=1 ENABLE_EXFAT=1

.PHONY: run-test
run-test: flash run
ifneq ($(UNAME),Windows)
	$(Q) ./check-disks.sh
endif
