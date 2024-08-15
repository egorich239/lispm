CFLAGS = -Wall -Werror -O

CLEANFILES =	    	\
	a.out				\
	demo				\
	demo.o				\
	lispm.o				\
	lispm.s				

.PHONY:	all
all:	demo

.PHONY:	clean
clean:;	$(RM) $(CLEANFILES)

demo: lispm.c demo.c

lispm.s: lispm.c symtable.h
	$(CC) $(CFLAGS) -march=i686 -mtune=i686 -m32 $@

symtable.h: gen-symtable.c
	$(CC) $(CFLAGS) gen-symtable.c
	./a.out > symtable.h
