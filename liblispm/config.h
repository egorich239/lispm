#pragma once

#ifndef LISPM_CONFIG_ASSERT
#define LISPM_CONFIG_ASSERT 1
#endif
#ifndef LISPM_CONFIG_VERBOSE
#define LISPM_CONFIG_VERBOSE 1
#endif

enum {
  /* Hash table uses open addressing.
     This value limits how many slots are looked up before we give up. */
  LISPM_CONFIG_HTABLE_LOOKUP_LIMIT =
#ifndef LISPM_CONFIG_HTABLE_LOOKUP_LIMIT_OVERRIDE
      32u
#else
      ((unsigned)(LISPM_CONFIG_HTABLE_LOOKUP_LIMIT_OVERRIDE))
#endif
  ,
};