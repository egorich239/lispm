TESTS = $(wildcard test-*.c)
LISPM_SRC = ../lispm.c ../lrt0.c ../rt-std.c ../debug.c ../lispm.ld
CFLAGS = -Wall -Werror -fomit-frame-pointer -gdwarf-2 -g3 -mtune=generic \
	-O0 -DLISPM_CONFIG_ASSERT=1 -DLISPM_CONFIG_VERBOSE=1 \
	-I.. -Wl,-T../lispm.ld \

CLEANFILES =

test-eval-tests.txt: $(wildcard eval/*)
	cat $^ >$@

test-eval-tests.o: test-eval-tests.txt
	$(LD) -r -b binary -z noexecstack -o $@ $^

CLEANFILES += test-eval-tests.txt test-eval-tests.o 
test-eval: test-eval.c test-eval-tests.o $(LISPM_SRC)
	$(CC) $(CFLAGS) $(filter %.o %.c,$^) -o $@

RUN_TESTS = $(foreach test,$(TESTS),$(patsubst %.c,%,run-$(test)))
.PHONY: $(RUN_TESTS)
$(RUN_TESTS): run-%: %
	./$^

.PHONY: clean
clean:
	rm $(CLEANFILES) $(patsubst %.c,%,$(TESTS))

all: $(RUN_TESTS) clean