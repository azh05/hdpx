#include <stdlib.h>
#include <R.h>

/* Safe malloc, raise an error on NULL return from malloc */

void* malloc_and_check(size_t x) {
  void *r = malloc(x);
  if (NULL == r) Rf_error("malloc failed");
  return r;
}
