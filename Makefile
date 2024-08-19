CFLAGS = -Wall -Werror -O0 -g -mtune=generic

SRCS = $(wildcard *.c)
OBJS = $(SRCS:%.c=%.o)

CLEANFILES =	    	\
	a.out				\
	$(OBJS)
	
.PHONY:	all
all:	c clean

.PHONY:	clean
clean:;	$(RM) $(CLEANFILES)

$(OBJS): %.o: %.c

c: c.o rt.o lispm.o

symtable.h: gen-symtable.c
	$(CC) $(CFLAGS) gen-symtable.c
	./a.out > symtable.h
