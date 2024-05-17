#include <stdint.h>
#include <stdio.h>
#include "runtime.h"

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
    printf("%ld", x);
}