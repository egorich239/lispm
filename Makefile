CFLAGS = -Wall -Werror -Oz -fomit-frame-pointer -g -mtune=generic

SRCS = $(wildcard *.c)
OBJS = $(SRCS:%.c=%.o)

CLEANFILES =	    	\
	a.out				\
	$(OBJS)				\
	c					\
	root.tar
	
.PHONY:	all
all:	c

.PHONY:	clean
clean:;	$(RM) $(CLEANFILES)

$(OBJS): %.o: %.c

c: c.o rt.o lispm.o debug.o support.o lrt0.c

.PHONY: root.tar
root.tar:
	tar -cf root.tar -C root 0

.PHONY: run
run: root.tar c
	./c root.tar

symtable.h: gen-symtable.c
	$(CC) $(CFLAGS) gen-symtable.c
	./a.out > symtable.h
