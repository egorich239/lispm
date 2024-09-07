PROJECT_ROOT := $(or $(PROJECT_ROOT),..)
BUILD_ROOT := $(or $(BUILD_ROOT),$(PROJECT_ROOT)/build)
SRC_DIR := $(shell realpath --relative-to=$(PROJECT_ROOT) $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
BUILD_DIR := $(BUILD_ROOT)/$(SRC_DIR)

TESTS := $(foreach f,$(wildcard test-*.c),$(basename $(f)))
RUN_TESTS := $(foreach t,$(TESTS),run-$(t))

test: $(RUN_TESTS)
.PHONY: test $(RUN_TESTS)

DEPS := \
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

$(BUILD_DIR):
	mkdir -p $@

.make-deps: | $(BUILD_DIR)
	$(MAKE) -C ../liblispm $(DEPS)
.PHONY: .make-deps

$(BUILD_DIR)/%.data.txt: %/* | $(BUILD_DIR)
	cat $^ >$@

$(BUILD_DIR)/%.data.o: $(BUILD_DIR)/%.data.txt
	cd $(dir $<); $(LD) -r -b binary -z noexecstack -o $(@F) $(<F)

$(BUILD_DIR)/test-%: test-%.c $(BUILD_DIR)/%.data.o | .make-deps
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

$(TESTS): %: $(BUILD_DIR)/%
.PHONY: $(TESTS)

$(RUN_TESTS): run-%: $(BUILD_DIR)/%
	$<
