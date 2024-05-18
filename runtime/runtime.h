#ifndef RUNTIME_H
#define RUNTIME_H

#include <stdint.h>
#include <stdio.h>
typedef int64_t value_t;
typedef void (*func_t)(value_t);

value_t allocTuple(int64_t n);
void halt(value_t x);
extern value_t GLOBAL_FUNC;
extern value_t GLOBAL_ARG;
value_t print(value_t);
void main_loop();

#endif