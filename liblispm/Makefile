PROJECT_ROOT := $(or $(PROJECT_ROOT),..)
BUILD_ROOT := $(or $(BUILD_ROOT),$(PROJECT_ROOT)/build)
SRC_DIR := $(shell realpath --relative-to=$(PROJECT_ROOT) $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
BUILD_DIR := $(BUILD_ROOT)/$(SRC_DIR)

CFLAGS = -Wall -Werror -fomit-frame-pointer -gdwarf-2 -g3 -mtune=generic -I$(PROJECT_ROOT)

all: make-00 make-01 make-10 make-11
.PHONY: all

$(BUILD_DIR):
	mkdir -p $@

$(BUILD_DIR)/%00.o: %.c | $(BUILD_DIR) $(wildcard %.h)
	$(CC) $(CFLAGS) -Oz -DLISPM_CONFIG_ASSERT=0 -DLISPM_CONFIG_VERBOSE=0 $< -c -o $@ 

$(BUILD_DIR)/%01.o: %.c | $(BUILD_DIR) $(wildcard %.h)
	$(CC) $(CFLAGS) -O0 -DLISPM_CONFIG_ASSERT=0 -DLISPM_CONFIG_VERBOSE=1 $< -c -o $@ 

$(BUILD_DIR)/%10.o: %.c | $(BUILD_DIR) $(wildcard %.h)
	$(CC) $(CFLAGS) -O0 -DLISPM_CONFIG_ASSERT=1 -DLISPM_CONFIG_VERBOSE=0 $< -c -o $@ 

$(BUILD_DIR)/%11.o: %.c | $(BUILD_DIR) $(wildcard %.h)
	$(CC) $(CFLAGS) -O0 -DLISPM_CONFIG_ASSERT=1 -DLISPM_CONFIG_VERBOSE=1 $< -c -o $@ 

# Syntax-only library, no builtin functions.
$(BUILD_DIR)/liblispm-syn%.a: $(BUILD_DIR)/lispm%.o
	$(AR) $(ARFLAGS) $@ $^ 

# Syntax and builtin functions.
$(BUILD_DIR)/liblispm-core%.a: $(BUILD_DIR)/lispm%.o $(BUILD_DIR)/funs%.o
	$(AR) $(ARFLAGS) $@ $^ 

# Standard C runtime environemnt.
$(BUILD_DIR)/liblispm-rtstd%.a: $(BUILD_DIR)/rtstd%.o
	$(AR) $(ARFLAGS) $@ $^ 

# Debugging functions.
$(BUILD_DIR)/liblispm-debug%.a: $(BUILD_DIR)/debug%.o
	$(AR) $(ARFLAGS) $@ $^

# Copy linker script.
$(BUILD_DIR)/lispm.ld: lispm.ld | $(BUILD_DIR)
	cp $< $@

make-00 make-01 make-10 make-11: make-%: $(BUILD_DIR)/liblispm-syn%.a $(BUILD_DIR)/liblispm-core%.a $(BUILD_DIR)/liblispm-debug%.a $(BUILD_DIR)/lispm.ld
.PHONY: make-%
