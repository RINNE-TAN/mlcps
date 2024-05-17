#ifndef RUNTIME_H
#define RUNTIME_H

#include <stdint.h>
typedef int64_t value_t;
typedef void (*func_t)(value_t);

value_t allocTuple(int64_t n);
void halt(value_t x);

#endif