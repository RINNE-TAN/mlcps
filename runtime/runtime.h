#ifndef RUNTIME_H
#define RUNTIME_H

#include <stdint.h>
#include <stdio.h>
typedef int64_t value_t;
typedef void (*func_t)();

value_t allocTuple(int64_t n);
void halt();
extern value_t GLOBAL_FUNC;
extern value_t REG_A0;
extern value_t REG_A1;
extern value_t REG_A2;
extern value_t REG_A3;
extern value_t REG_A4;
extern value_t REG_A5;
extern value_t REG_A6;
extern value_t REG_A7;
value_t print(value_t);
void main_loop();

#endif