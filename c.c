#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lispm.h"
#include "symprint.h"
#include "symtable.h"

__attribute__((aligned(PAGE_ALIGN))) char TABLE[TABLE_SIZE] = TABLE_PREFIX;
Sym STACK[STACK_SIZE];

extern void Update(const char *lit, const char *hidden, unsigned align);
extern void lispm_init(void);
extern Sym lispm_start(void);

static char TEXT[1048576];

int main() {
  int r = fread(TEXT, 1, sizeof(TEXT) - 1, stdin);
  if (r < 0) return r;

  lispm_init();
  Update("/0", TEXT, PAGE_ALIGN_LOG2);
  Sym result = lispm_start();
  lispm_dump(result);
}