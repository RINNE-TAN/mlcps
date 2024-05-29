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
    REG_BP = REG_SP;
    REG_SP += sizeof(value_t) * 2;
    REG_T0 = REG_SP;
    REG_SP += sizeof(value_t) * 0;
    REG_T1 = REG_SP;
    ((value_t *)REG_SP)[0] = (value_t)fCode18;
    ((value_t *)REG_SP)[1] = REG_T0;
    REG_SP += sizeof(value_t) * 2;
    REG_T0 = 10;
    REG_T2 = REG_SP;
    REG_SP += sizeof(value_t) * 0;
    ((value_t *)REG_BP)[0] = REG_SP;
    ((value_t *)REG_SP)[0] = (value_t)kCode51;
    ((value_t *)REG_SP)[1] = REG_T2;
    REG_SP += sizeof(value_t) * 2;
    REG_T2 = ((value_t *)REG_T1)[0];
    ((value_t *)REG_BP)[1] = ((value_t *)REG_T1)[1];
    GLOBAL_FUNC = REG_T2;
    REG_A0 = ((value_t *)REG_BP)[1];
    REG_A1 = ((value_t *)REG_BP)[0];
    REG_A2 = REG_T0;
}
void kCode37()
{
    REG_BP = REG_SP;
    REG_SP += sizeof(value_t) * 1;
    REG_T0 = ((value_t *)REG_A0)[0];
    REG_T1 = ((value_t *)REG_A0)[1];
    REG_T2 = REG_T1 + REG_A1;
    REG_T1 = ((value_t *)REG_T0)[0];
    ((value_t *)REG_BP)[0] = ((value_t *)REG_T0)[1];
    GLOBAL_FUNC = REG_T1;
    REG_A0 = ((value_t *)REG_BP)[0];
    REG_A1 = REG_T2;
}
void kCode34()
{
    REG_BP = REG_SP;
    REG_SP += sizeof(value_t) * 2;
    REG_T0 = ((value_t *)REG_A0)[0];
    ((value_t *)REG_BP)[0] = ((value_t *)REG_A0)[1];
    REG_T2 = ((value_t *)REG_A0)[2];
    REG_T1 = 1;
    ((value_t *)REG_BP)[1] = REG_T2 - REG_T1;
    REG_T1 = REG_SP;
    ((value_t *)REG_SP)[0] = REG_T0;
    ((value_t *)REG_SP)[1] = REG_T2;
    REG_SP += sizeof(value_t) * 2;
    REG_T0 = REG_SP;
    ((value_t *)REG_SP)[0] = (value_t)kCode37;
    ((value_t *)REG_SP)[1] = REG_T1;
    REG_SP += sizeof(value_t) * 2;
    REG_T1 = ((value_t *)((value_t *)REG_BP)[0])[0];
    REG_T2 = ((value_t *)((value_t *)REG_BP)[0])[1];
    GLOBAL_FUNC = REG_T1;
    REG_A0 = REG_T2;
    REG_A1 = REG_T0;
    REG_A2 = ((value_t *)REG_BP)[1];
}
void kCode31()
{
    REG_BP = REG_SP;
    REG_SP += sizeof(value_t) * 2;
    REG_T0 = ((value_t *)REG_A0)[0];
    REG_T1 = ((value_t *)REG_A0)[1];
    ((value_t *)REG_BP)[0] = ((value_t *)REG_A0)[2];
    REG_T2 = REG_SP;
    ((value_t *)REG_SP)[0] = REG_T0;
    ((value_t *)REG_SP)[1] = REG_T1;
    ((value_t *)REG_SP)[2] = ((value_t *)REG_BP)[0];
    REG_SP += sizeof(value_t) * 3;
    REG_T0 = REG_SP;
    ((value_t *)REG_SP)[0] = (value_t)kCode34;
    ((value_t *)REG_SP)[1] = REG_T2;
    REG_SP += sizeof(value_t) * 2;
    REG_T1 = print(((value_t *)REG_BP)[0]);
    REG_T2 = ((value_t *)REG_T0)[0];
    ((value_t *)REG_BP)[1] = ((value_t *)REG_T0)[1];
    GLOBAL_FUNC = REG_T2;
    REG_A0 = ((value_t *)REG_BP)[1];
    REG_A1 = REG_T1;
}
void kCode26()
{
    REG_BP = REG_SP;
    REG_SP += sizeof(value_t) * 1;
    REG_T0 = ((value_t *)REG_A0)[0];
    REG_T1 = 0;
    REG_T2 = ((value_t *)REG_T0)[0];
    ((value_t *)REG_BP)[0] = ((value_t *)REG_T0)[1];
    GLOBAL_FUNC = REG_T2;
    REG_A0 = ((value_t *)REG_BP)[0];
    REG_A1 = REG_T1;
}
void kCode21()
{
    REG_BP = REG_SP;
    REG_SP += sizeof(value_t) * 0;
    REG_T0 = ((value_t *)REG_A0)[0];
    REG_T1 = ((value_t *)REG_T0)[0];
    REG_T2 = ((value_t *)REG_T0)[1];
    GLOBAL_FUNC = REG_T1;
    REG_A0 = REG_T2;
    REG_A1 = REG_A1;
}
void kCode51()
{
    REG_BP = REG_SP;
    REG_SP += sizeof(value_t) * 0;
    GLOBAL_FUNC = (value_t)halt;
    REG_A0 = REG_A1;
}
void fCode18()
{
    REG_BP = REG_SP;
    REG_SP += sizeof(value_t) * 4;
    REG_T0 = REG_SP;
    ((value_t *)REG_SP)[0] = (value_t)fCode18;
    ((value_t *)REG_SP)[1] = REG_A0;
    REG_SP += sizeof(value_t) * 2;
    REG_T1 = REG_SP;
    ((value_t *)REG_SP)[0] = REG_A1;
    REG_SP += sizeof(value_t) * 1;
    REG_T2 = REG_SP;
    ((value_t *)REG_SP)[0] = (value_t)kCode21;
    ((value_t *)REG_SP)[1] = REG_T1;
    REG_SP += sizeof(value_t) * 2;
    REG_T1 = REG_SP;
    ((value_t *)REG_SP)[0] = REG_T2;
    REG_SP += sizeof(value_t) * 1;
    ((value_t *)REG_BP)[0] = REG_SP;
    ((value_t *)REG_SP)[0] = (value_t)kCode26;
    ((value_t *)REG_SP)[1] = REG_T1;
    REG_SP += sizeof(value_t) * 2;
    REG_T1 = REG_SP;
    ((value_t *)REG_SP)[0] = REG_T2;
    ((value_t *)REG_SP)[1] = REG_T0;
    ((value_t *)REG_SP)[2] = REG_A2;
    REG_SP += sizeof(value_t) * 3;
    REG_T0 = REG_SP;
    ((value_t *)REG_SP)[0] = (value_t)kCode31;
    ((value_t *)REG_SP)[1] = REG_T1;
    REG_SP += sizeof(value_t) * 2;
    REG_T1 = (value_t)NULL;
    REG_T2 = ((value_t *)((value_t *)REG_BP)[0])[0];
    ((value_t *)REG_BP)[1] = ((value_t *)((value_t *)REG_BP)[0])[1];
    ((value_t *)REG_BP)[2] = ((value_t *)REG_T0)[0];
    ((value_t *)REG_BP)[3] = ((value_t *)REG_T0)[1];
    if (REG_A2 == 0)
    {
        GLOBAL_FUNC = REG_T2;
        REG_A0 = ((value_t *)REG_BP)[1];
        REG_A1 = REG_T1;
    }
    else
    {
        GLOBAL_FUNC = ((value_t *)REG_BP)[2];
        REG_A0 = ((value_t *)REG_BP)[3];
        REG_A1 = REG_T1;
    }
}
int main()
{
    GLOBAL_FUNC = (value_t)main56;
    main_loop();
    return 0;
}