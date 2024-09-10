export PROJECT_ROOT := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
export BUILD_ROOT := $(PROJECT_ROOT)/build

all: liblispm repl test | $(BUILD_ROOT)
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

repl:
	$(MAKE) -C repl
.PHONY: repl

$(BUILD_ROOT):
	mkdir -p $@

STAT_DEPS := \
	$(BUILD_ROOT)/liblispm/lispm00.o \
	$(BUILD_ROOT)/liblispm/funs00.o
stat: stat.sh
	$(MAKE) -C liblispm $(STAT_DEPS)
	./$< $(STAT_DEPS)
.PHONY: stat
