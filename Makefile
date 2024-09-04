CFLAGS = -Wall -Werror -fomit-frame-pointer -gdwarf-2 -g3 -mtune=generic

SRCS = $(wildcard *.c)

CLEANFILES = launcher
	
.PHONY:	all
all:	launcher stat

.PHONY:	clean
clean:
	$(RM) $(CLEANFILES)
	$(MAKE) -C tests clean

.PHONY: stat
stat: CFLAGS += -c -Oz -DLISPM_CONFIG_ASSERT=0 -DLISPM_CONFIG_VERBOSE=0
stat: lispm.c lrt0.c lispm-funs.c stat.sh
	$(CC) $(CFLAGS) $(filter %.c,$^)
	./stat.sh
	rm $(patsubst %.c,%.o,$(filter %.c,$^))

launcher: CFLAGS += -O0 -Wl,-T$(filter %.ld,$^)
launcher: launcher.c lrt0.c lispm.c lispm-funs.c rt-std.c debug.c lispm.ld
	$(CC) $(CFLAGS) $(filter %.c,$^) -o $@

.PHONY: test
test:
	$(MAKE) -C tests all