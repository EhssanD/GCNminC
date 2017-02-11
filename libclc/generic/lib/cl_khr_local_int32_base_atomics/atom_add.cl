#include <clc/clc.h>

#define IMPL(TYPE) \
_CLC_OVERLOAD _CLC_DEF TYPE atom_add(local TYPE *p, TYPE val) { \
  return atomic_add(p, val); \
}

IMPL(int)
IMPL(unsigned int)
