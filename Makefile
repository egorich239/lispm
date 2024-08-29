CFLAGS = -Wall -Werror -O0 -fomit-frame-pointer -gdwarf-2 -g3 -mtune=generic

SRCS = $(wildcard *.c)
OBJS = $(SRCS:%.c=%.o)

CLEANFILES = $(OBJS) a.out launcher testeval evaltests.txt evaltests.o
	
.PHONY:	all
all:	launcher

.PHONY:	clean
clean:;	$(RM) $(CLEANFILES)

$(OBJS): %.o: %.c

launcher: launcher.o lrt0.o lispm.o rt-std.o debug.o lispm.ld
	$(CC) $(CFLAGS) $(filter %.o,$^) -Wl,-T$(filter %.ld,$^) -o $@

evaltests.txt: $(wildcard evaltests/*)
	cat $^ >$@

evaltests.o: evaltests.txt
	$(LD) -r -b binary -z noexecstack -o $@ $^

testeval: testeval.c evaltests.o lrt0.o lispm.o rt-std.o debug.o lispm.ld
	$(CC) $(CFLAGS) $(filter %.o %.c,$^) -Wl,-T$(filter %.ld,$^) -o $@

.PHONY: test
test: testeval
	./testeval