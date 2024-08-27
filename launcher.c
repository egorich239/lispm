#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include "debug.h"
#include "lispm.h"

extern struct Builtin lispm_section_builtins;

Sym stack[4 * 1024 * 1024];
char strings[16 * 1024 * 1024];
unsigned htable[4 * 1024 * 1024];

struct Lispm lispm = {
    .builtins = &lispm_section_builtins,

    /*stack*/
    .stack = stack,
    .sp = stack + (sizeof(stack) / sizeof(*stack)),
    .pp = stack + 64,

    /*strings*/
    .strings = strings,
    .strings_end = strings + sizeof(strings),
    .tp = strings + LISPM_DIAG_SIZE,

    /*program*/
    .program = 0,
    .program_end = 0,
    .pc = 0,

    /*htable*/
    .htable = htable,
    .htable_end = htable + (sizeof(htable) / sizeof(*htable)),
};

int main(int argc, char *argv[]) {
  if (argc < 2) return 1;

  int fd = open(argv[1], O_RDONLY);
  struct stat stat;
  if (fd < 0 || fstat(fd, &stat) < 0) return 1;

  void *page_begin = mmap(NULL, stat.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
  if (!page_begin) return 1;

  lispm.program = page_begin;
  lispm.program_end =page_begin + stat.st_size;
  lispm.pc = lispm.program;

  Sym result = lispm_exec();
  fprintf(stderr, "%s\n", lispm_error_message_get());
  lispm_dump(result);
}