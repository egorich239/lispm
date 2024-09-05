export PROJECT_ROOT := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
export BUILD_ROOT := $(PROJECT_ROOT)/build

all: liblispm launcher test | $(BUILD_ROOT)
.PHONY:	all

clean:
	$(RM) -r $(BUILD_ROOT) launcher
.PHONY:	clean

liblispm:
	$(MAKE) -C liblispm
.PHONY: liblispm

test:
	$(MAKE) -C tests
	$(MAKE) stat
.PHONY: test

$(BUILD_ROOT):
	mkdir -p $@

STAT_DEPS := \
	$(BUILD_ROOT)/liblispm/lispm00.o \
	$(BUILD_ROOT)/liblispm/funs00.o
stat: stat.sh
	$(MAKE) -C liblispm $(STAT_DEPS)
	./$< $(STAT_DEPS)
.PHONY: stat

LAUNCHER_DEPS := \
	$(BUILD_ROOT)/liblispm/liblispm-core11.a \
	$(BUILD_ROOT)/liblispm/liblispm-rtstd11.a \
	$(BUILD_ROOT)/liblispm/liblispm-debug11.a \
	$(BUILD_ROOT)/liblispm/lispm.ld
CFLAGS := -Wall -Werror -fomit-frame-pointer -gdwarf-2 -g3 -mtune=generic \
	-O0 -DLISPM_CONFIG_ASSERT=1 -DLISPM_CONFIG_VERBOSE=1 \
	-I$(PROJECT_ROOT)
LDFLAGS := \
	-L$(BUILD_ROOT)/liblispm \
	-Wl,--whole-archive \
	-llispm-core11 \
	-Wl,--no-whole-archive \
	-llispm-rtstd11 -llispm-debug11 \
	-Wl,-T$(BUILD_ROOT)/liblispm/lispm.ld

launcher: launcher.c
	$(MAKE) -C liblispm $(LAUNCHER_DEPS)
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

