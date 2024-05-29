#include <stdint.h>
#include <stdio.h>
#include "runtime.h"
value_t GLOBAL_FUNC = (value_t)NULL;
value_t REG_A0 = (value_t)NULL;
value_t REG_A1 = (value_t)NULL;
value_t REG_A2 = (value_t)NULL;
value_t REG_A3 = (value_t)NULL;
value_t REG_A4 = (value_t)NULL;
value_t REG_A5 = (value_t)NULL;
value_t REG_A6 = (value_t)NULL;
value_t REG_A7 = (value_t)NULL;
value_t REG_T0 = (value_t)NULL;
value_t REG_T1 = (value_t)NULL;
value_t REG_T2 = (value_t)NULL;
value_t REG_T3 = (value_t)NULL;
value_t REG_T4 = (value_t)NULL;
value_t REG_T5 = (value_t)NULL;
value_t REG_T6 = (value_t)NULL;
value_t REG_T7 = (value_t)NULL;
#define MEM_SIZE 4096
value_t allocTuple(int64_t n)
{
    if (n == 0)
    {
        return (value_t)NULL;
    }
    static value_t mem[MEM_SIZE] = {0};
    static int idx = 0;
    value_t ptr = (value_t)&mem[idx];
    idx += n;
    return ptr;
}
void halt()
{
    printf("halt with result: %ld\n", REG_A0);
    GLOBAL_FUNC = (value_t)NULL;
}

void main_loop()
{
    while (GLOBAL_FUNC)
    {
        ((func_t)GLOBAL_FUNC)();
    }
}
value_t print(value_t x)
{
    printf("print: %ld\n", x);
    return 0;
}