CFLAGS = -Wall -Werror -O0 -g -mtune=generic

CLEANFILES =	    	\
	a.out				\
	c					\
	lispm.o				\
	lispm.s				

.PHONY:	all
all:	c

.PHONY:	clean
clean:;	$(RM) $(CLEANFILES)

c: lispm.c c.c

# lispm.o: lispm.c
# 	$(CC) -c -Os -march=i686 -mtune=i686 -m32 -fomit-frame-pointer lispm.c -o lispm.o

symtable.h: gen-symtable.c
	$(CC) $(CFLAGS) gen-symtable.c
	./a.out > symtable.h
