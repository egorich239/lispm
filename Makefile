CFLAGS = -Wall -Werror -O0 -fomit-frame-pointer -g -mtune=generic

SRCS = $(wildcard *.c)
OBJS = $(SRCS:%.c=%.o)

CLEANFILES = $(OBJS) a.out c root.tar
	
.PHONY:	all
all:	c

.PHONY:	clean
clean:;	$(RM) $(CLEANFILES)

$(OBJS): %.o: %.c

c: c.o lrt0.o lispm.o rt.o debug.o support.o

.PHONY: root.tar
root.tar:
	tar -cf root.tar -C root $(shell find root -type f -printf '%P\n' | sort)

.PHONY: run
run: root.tar c
	./c root.tar

symtable.h: gen-symtable.c
	$(CC) $(CFLAGS) gen-symtable.c
	./a.out > symtable.h
