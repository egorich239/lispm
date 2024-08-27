CFLAGS = -Wall -Werror -O0 -fomit-frame-pointer -gdwarf-2 -g3 -mtune=generic

SRCS = $(wildcard *.c)
OBJS = $(SRCS:%.c=%.o)

CLEANFILES = $(OBJS) a.out launcher launcher.ld
	
.PHONY:	all
all:	launcher

.PHONY:	clean
clean:;	$(RM) $(CLEANFILES)

$(OBJS): %.o: %.c

launcher.ld: gen-launcherld
	CC=$(CC) ./gen-launcherld $@


launcher: launcher.o lrt0.o lispm.o rt-std.o debug.o launcher.ld
	$(CC) $(CFLAGS) $(filter %.o,$^) -T $(filter %.ld,$^) -o $@

.PHONY: run
run: launcher tests
	./launcher tests

symtable.h: gen-symtable.c
	$(CC) $(CFLAGS) gen-symtable.c
	./a.out > symtable.h
