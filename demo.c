#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lispm.h"
#include "symtable.h"

char TABLE[TABLE_SIZE] = TABLE_PREFIX;
Sym STACK[STACK_SIZE];
unsigned stack_pointer;

void lispm_halt(Sym err) {
  fprintf(stderr, "err = %04x\nstack = [...", err);
  for (unsigned i = 0; i < 32; ++i) {
    fprintf(stderr, " %04x", STACK[STACK_SIZE - 32 + i]);
  }
  fprintf(stderr, "]\nTABLE:\n");
  for (unsigned i = 0; i < TABLE_SIZE;) {
    while (i < TABLE_SIZE && !TABLE[i++])
      ;
    if (i == TABLE_SIZE)
      break;
    fprintf(stderr, " %04x %s\n", i - 1, TABLE + i - 1);
    while (i < TABLE_SIZE && TABLE[i++])
      ;
  }
  exit(1);
}

void lispm_done(void) { exit(0); }

extern void lispm_start(void);

int main() {
  memcpy(TABLE + 512, "'A", 3);
  lispm_start();
  lispm_halt(0);
}