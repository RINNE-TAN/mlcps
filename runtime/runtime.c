#include <stdint.h>
#include <stdio.h>
#include "runtime.h"
value_t GLOBAL_FUNC = (value_t)NULL;
value_t GLOBAL_ARG = (value_t)NULL;
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
void halt(value_t x)
{
    printf("halt with result: %ld\n", x);
    GLOBAL_FUNC = (value_t)NULL;
    GLOBAL_ARG = (value_t)NULL;
}

void main_loop()
{
    while (GLOBAL_FUNC)
    {
        ((func_t)GLOBAL_FUNC)(GLOBAL_ARG);
    }
}