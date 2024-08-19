#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include "lispm.h"
#include "symprint.h"

extern Sym lispm_start(struct Page *program);

int main(int argc, char *argv[]) {
  if (argc < 2) return 1;

  int fd = open(argv[1], O_RDONLY);
  struct stat stat;
  if (fd < 0 || fstat(fd, &stat) < 0) return 1;

  void *page_begin = mmap(NULL, stat.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
  if (!page_begin) return 1;

  struct Page program;
  program.begin = page_begin;
  program.end = program.begin + stat.st_size;
  Sym result = lispm_start(&program);
  lispm_dump(result);
}