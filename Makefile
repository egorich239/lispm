CFLAGS = -Wall -Werror -Oz -fomit-frame-pointer -gdwarf-2 -g3 -mtune=generic

SRCS = $(wildcard *.c)
OBJS = $(SRCS:%.c=%.o)

CLEANFILES = $(OBJS) a.out launcher
	
.PHONY:	all
all:	launcher

.PHONY:	clean
clean:;	$(RM) $(CLEANFILES)

$(OBJS): %.o: %.c

launcher: launcher.o lrt0.o lispm.o rt-std.o debug.o lispm.ld
	$(CC) $(CFLAGS) $(filter %.o,$^) -Wl,-T$(filter %.ld,$^) -o $@

.PHONY: run
run: launcher tests
	./launcher tests

symtable.h: gen-symtable.c
	$(CC) $(CFLAGS) gen-symtable.c
	./a.out > symtable.h
