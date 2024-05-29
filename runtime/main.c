#include <stdio.h>
#include "runtime.h"
void main56();
void kCode37();
void kCode34();
void kCode31();
void kCode26();
void kCode21();
void kCode51();
void fCode18();
void main56()
{
    REG_T2 = allocTuple(3);
    REG_T0 = allocTuple(0);
    REG_T1 = allocTuple(2);
    ((value_t *)REG_T1)[0] = (value_t)fCode18;
    ((value_t *)REG_T1)[1] = REG_T0;
    ((value_t *)REG_T2)[0] = 10;
    REG_T0 = allocTuple(0);
    ((value_t *)REG_T2)[1] = allocTuple(2);
    ((value_t *)((value_t *)REG_T2)[1])[0] = (value_t)kCode51;
    ((value_t *)((value_t *)REG_T2)[1])[1] = REG_T0;
    REG_T0 = ((value_t *)REG_T1)[0];
    ((value_t *)REG_T2)[2] = ((value_t *)REG_T1)[1];
    GLOBAL_FUNC = REG_T0;
    REG_A0 = (value_t)((value_t *)REG_T2)[2];
    REG_A1 = (value_t)((value_t *)REG_T2)[1];
    REG_A2 = (value_t)((value_t *)REG_T2)[0];
}
void kCode37()
{
    REG_T2 = allocTuple(2);
    REG_T0 = ((value_t *)REG_A0)[0];
    REG_T1 = ((value_t *)REG_A0)[1];
    ((value_t *)REG_T2)[0] = REG_T1 + REG_A1;
    REG_T1 = ((value_t *)REG_T0)[0];
    ((value_t *)REG_T2)[1] = ((value_t *)REG_T0)[1];
    GLOBAL_FUNC = REG_T1;
    REG_A0 = (value_t)((value_t *)REG_T2)[1];
    REG_A1 = (value_t)((value_t *)REG_T2)[0];
}
void kCode34()
{
    REG_T2 = allocTuple(4);
    REG_T0 = ((value_t *)REG_A0)[0];
    ((value_t *)REG_T2)[0] = ((value_t *)REG_A0)[1];
    ((value_t *)REG_T2)[1] = ((value_t *)REG_A0)[2];
    REG_T1 = 1;
    ((value_t *)REG_T2)[2] = ((value_t *)REG_T2)[1] - REG_T1;
    REG_T1 = allocTuple(2);
    ((value_t *)REG_T1)[0] = REG_T0;
    ((value_t *)REG_T1)[1] = ((value_t *)REG_T2)[1];
    REG_T0 = allocTuple(2);
    ((value_t *)REG_T0)[0] = (value_t)kCode37;
    ((value_t *)REG_T0)[1] = REG_T1;
    REG_T1 = ((value_t *)((value_t *)REG_T2)[0])[0];
    ((value_t *)REG_T2)[3] = ((value_t *)((value_t *)REG_T2)[0])[1];
    GLOBAL_FUNC = REG_T1;
    REG_A0 = (value_t)((value_t *)REG_T2)[3];
    REG_A1 = (value_t)REG_T0;
    REG_A2 = (value_t)((value_t *)REG_T2)[2];
}
void kCode31()
{
    REG_T2 = allocTuple(4);
    REG_T0 = ((value_t *)REG_A0)[0];
    REG_T1 = ((value_t *)REG_A0)[1];
    ((value_t *)REG_T2)[0] = ((value_t *)REG_A0)[2];
    ((value_t *)REG_T2)[1] = allocTuple(3);
    ((value_t *)((value_t *)REG_T2)[1])[0] = REG_T0;
    ((value_t *)((value_t *)REG_T2)[1])[1] = REG_T1;
    ((value_t *)((value_t *)REG_T2)[1])[2] = ((value_t *)REG_T2)[0];
    REG_T0 = allocTuple(2);
    ((value_t *)REG_T0)[0] = (value_t)kCode34;
    ((value_t *)REG_T0)[1] = ((value_t *)REG_T2)[1];
    REG_T1 = print(((value_t *)REG_T2)[0]);
    ((value_t *)REG_T2)[2] = ((value_t *)REG_T0)[0];
    ((value_t *)REG_T2)[3] = ((value_t *)REG_T0)[1];
    GLOBAL_FUNC = ((value_t *)REG_T2)[2];
    REG_A0 = (value_t)((value_t *)REG_T2)[3];
    REG_A1 = (value_t)REG_T1;
}
void kCode26()
{
    REG_T2 = allocTuple(2);
    REG_T0 = ((value_t *)REG_A0)[0];
    REG_T1 = 0;
    ((value_t *)REG_T2)[0] = ((value_t *)REG_T0)[0];
    ((value_t *)REG_T2)[1] = ((value_t *)REG_T0)[1];
    GLOBAL_FUNC = ((value_t *)REG_T2)[0];
    REG_A0 = (value_t)((value_t *)REG_T2)[1];
    REG_A1 = (value_t)REG_T1;
}
void kCode21()
{
    REG_T2 = allocTuple(1);
    REG_T0 = ((value_t *)REG_A0)[0];
    REG_T1 = ((value_t *)REG_T0)[0];
    ((value_t *)REG_T2)[0] = ((value_t *)REG_T0)[1];
    GLOBAL_FUNC = REG_T1;
    REG_A0 = (value_t)((value_t *)REG_T2)[0];
    REG_A1 = (value_t)REG_A1;
}
void kCode51()
{
    REG_T2 = allocTuple(0);
    GLOBAL_FUNC = (value_t)halt;
    REG_A0 = REG_A1;
}
void fCode18()
{
    REG_T2 = allocTuple(6);
    REG_T0 = allocTuple(2);
    ((value_t *)REG_T0)[0] = (value_t)fCode18;
    ((value_t *)REG_T0)[1] = REG_A0;
    REG_T1 = allocTuple(1);
    ((value_t *)REG_T1)[0] = REG_A1;
    ((value_t *)REG_T2)[0] = allocTuple(2);
    ((value_t *)((value_t *)REG_T2)[0])[0] = (value_t)kCode21;
    ((value_t *)((value_t *)REG_T2)[0])[1] = REG_T1;
    REG_T1 = allocTuple(1);
    ((value_t *)REG_T1)[0] = ((value_t *)REG_T2)[0];
    ((value_t *)REG_T2)[1] = allocTuple(2);
    ((value_t *)((value_t *)REG_T2)[1])[0] = (value_t)kCode26;
    ((value_t *)((value_t *)REG_T2)[1])[1] = REG_T1;
    REG_T1 = allocTuple(3);
    ((value_t *)REG_T1)[0] = ((value_t *)REG_T2)[0];
    ((value_t *)REG_T1)[1] = REG_T0;
    ((value_t *)REG_T1)[2] = REG_A2;
    REG_T0 = allocTuple(2);
    ((value_t *)REG_T0)[0] = (value_t)kCode31;
    ((value_t *)REG_T0)[1] = REG_T1;
    REG_T1 = (value_t)NULL;
    ((value_t *)REG_T2)[2] = ((value_t *)((value_t *)REG_T2)[1])[0];
    ((value_t *)REG_T2)[3] = ((value_t *)((value_t *)REG_T2)[1])[1];
    ((value_t *)REG_T2)[4] = ((value_t *)REG_T0)[0];
    ((value_t *)REG_T2)[5] = ((value_t *)REG_T0)[1];
    if (REG_A2 == 0)
    {
        GLOBAL_FUNC = ((value_t *)REG_T2)[2];
        REG_A0 = (value_t)((value_t *)REG_T2)[3];
        REG_A1 = (value_t)REG_T1;
    }
    else
    {
        GLOBAL_FUNC = ((value_t *)REG_T2)[4];
        REG_A0 = (value_t)((value_t *)REG_T2)[5];
        REG_A1 = (value_t)REG_T1;
    }
}
int main()
{
    GLOBAL_FUNC = (value_t)main56;
    main_loop();
    return 0;
}