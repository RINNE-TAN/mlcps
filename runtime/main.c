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
void block57();
void block58();
void main56()
{
    REG_S0 = REG_SP + 0;
    REG_SP = REG_SP + 16;
    REG_T1 = REG_SP + 0;
    REG_SP = REG_SP + 16;
    REG_L0 = (value_t)fCode18;
    ((value_t *)REG_T1)[0] = REG_L0;
    ((value_t *)REG_T1)[1] = REG_T0;
    REG_T0 = REG_ZERO + 10;
    REG_L1 = REG_SP + 0;
    REG_SP = REG_SP + 16;
    REG_L0 = (value_t)kCode51;
    ((value_t *)REG_L1)[0] = REG_L0;
    ((value_t *)REG_L1)[1] = REG_T2;
    ((value_t *)REG_S0)[0] = REG_L1;
    REG_T2 = ((value_t *)REG_T1)[0];
    REG_L1 = ((value_t *)REG_T1)[1];
    ((value_t *)REG_S0)[1] = REG_L1;
    REG_L0 = ((value_t *)REG_S0)[1];
    REG_A0 = REG_L0 + 0;
    REG_L0 = ((value_t *)REG_S0)[0];
    REG_A1 = REG_L0 + 0;
    REG_A2 = REG_T0 + 0;
    GLOBAL_FUNC = REG_T2;
}
void kCode37()
{
    REG_S0 = REG_SP + 0;
    REG_SP = REG_SP + 8;
    REG_T0 = ((value_t *)REG_A0)[0];
    REG_T1 = ((value_t *)REG_A0)[1];
    REG_T2 = REG_T1 + REG_A1;
    REG_T1 = ((value_t *)REG_T0)[0];
    REG_L1 = ((value_t *)REG_T0)[1];
    ((value_t *)REG_S0)[0] = REG_L1;
    REG_L0 = ((value_t *)REG_S0)[0];
    REG_A0 = REG_L0 + 0;
    REG_A1 = REG_T2 + 0;
    GLOBAL_FUNC = REG_T1;
}
void kCode34()
{
    REG_S0 = REG_SP + 0;
    REG_SP = REG_SP + 16;
    REG_T0 = ((value_t *)REG_A0)[0];
    REG_L1 = ((value_t *)REG_A0)[1];
    ((value_t *)REG_S0)[0] = REG_L1;
    REG_T2 = ((value_t *)REG_A0)[2];
    REG_T1 = REG_ZERO + 1;
    REG_L1 = REG_T2 - REG_T1;
    ((value_t *)REG_S0)[1] = REG_L1;
    REG_T1 = REG_SP + 0;
    REG_SP = REG_SP + 16;
    ((value_t *)REG_T1)[0] = REG_T0;
    ((value_t *)REG_T1)[1] = REG_T2;
    REG_T0 = REG_SP + 0;
    REG_SP = REG_SP + 16;
    REG_L0 = (value_t)kCode37;
    ((value_t *)REG_T0)[0] = REG_L0;
    ((value_t *)REG_T0)[1] = REG_T1;
    REG_L0 = ((value_t *)REG_S0)[0];
    REG_T1 = ((value_t *)REG_L0)[0];
    REG_L0 = ((value_t *)REG_S0)[0];
    REG_T2 = ((value_t *)REG_L0)[1];
    REG_A0 = REG_T2 + 0;
    REG_A1 = REG_T0 + 0;
    REG_L0 = ((value_t *)REG_S0)[1];
    REG_A2 = REG_L0 + 0;
    GLOBAL_FUNC = REG_T1;
}
void kCode31()
{
    REG_S0 = REG_SP + 0;
    REG_SP = REG_SP + 16;
    REG_T0 = ((value_t *)REG_A0)[0];
    REG_T1 = ((value_t *)REG_A0)[1];
    REG_L1 = ((value_t *)REG_A0)[2];
    ((value_t *)REG_S0)[0] = REG_L1;
    REG_T2 = REG_SP + 0;
    REG_SP = REG_SP + 24;
    ((value_t *)REG_T2)[0] = REG_T0;
    ((value_t *)REG_T2)[1] = REG_T1;
    REG_L0 = ((value_t *)REG_S0)[0];
    ((value_t *)REG_T2)[2] = REG_L0;
    REG_T0 = REG_SP + 0;
    REG_SP = REG_SP + 16;
    REG_L0 = (value_t)kCode34;
    ((value_t *)REG_T0)[0] = REG_L0;
    ((value_t *)REG_T0)[1] = REG_T2;
    REG_L0 = ((value_t *)REG_S0)[0];
    print(REG_L0);
    REG_T2 = ((value_t *)REG_T0)[0];
    REG_L1 = ((value_t *)REG_T0)[1];
    ((value_t *)REG_S0)[1] = REG_L1;
    REG_L0 = ((value_t *)REG_S0)[1];
    REG_A0 = REG_L0 + 0;
    REG_A1 = REG_T1 + 0;
    GLOBAL_FUNC = REG_T2;
}
void kCode26()
{
    REG_S0 = REG_SP + 0;
    REG_SP = REG_SP + 8;
    REG_T0 = ((value_t *)REG_A0)[0];
    REG_T1 = REG_ZERO + 0;
    REG_T2 = ((value_t *)REG_T0)[0];
    REG_L1 = ((value_t *)REG_T0)[1];
    ((value_t *)REG_S0)[0] = REG_L1;
    REG_L0 = ((value_t *)REG_S0)[0];
    REG_A0 = REG_L0 + 0;
    REG_A1 = REG_T1 + 0;
    GLOBAL_FUNC = REG_T2;
}
void kCode21()
{
    REG_T0 = ((value_t *)REG_A0)[0];
    REG_T1 = ((value_t *)REG_T0)[0];
    REG_T2 = ((value_t *)REG_T0)[1];
    REG_A0 = REG_T2 + 0;
    REG_A1 = REG_A1 + 0;
    GLOBAL_FUNC = REG_T1;
}
void kCode51()
{
    REG_A0 = REG_A1 + 0;
    GLOBAL_FUNC = (value_t)halt;
}
void fCode18()
{
    REG_S0 = REG_SP + 0;
    REG_SP = REG_SP + 32;
    REG_T0 = REG_SP + 0;
    REG_SP = REG_SP + 16;
    REG_L0 = (value_t)fCode18;
    ((value_t *)REG_T0)[0] = REG_L0;
    ((value_t *)REG_T0)[1] = REG_A0;
    REG_T1 = REG_SP + 0;
    REG_SP = REG_SP + 8;
    ((value_t *)REG_T1)[0] = REG_A1;
    REG_T2 = REG_SP + 0;
    REG_SP = REG_SP + 16;
    REG_L0 = (value_t)kCode21;
    ((value_t *)REG_T2)[0] = REG_L0;
    ((value_t *)REG_T2)[1] = REG_T1;
    REG_T1 = REG_SP + 0;
    REG_SP = REG_SP + 8;
    ((value_t *)REG_T1)[0] = REG_T2;
    REG_L1 = REG_SP + 0;
    REG_SP = REG_SP + 16;
    REG_L0 = (value_t)kCode26;
    ((value_t *)REG_L1)[0] = REG_L0;
    ((value_t *)REG_L1)[1] = REG_T1;
    ((value_t *)REG_S0)[0] = REG_L1;
    REG_T1 = REG_SP + 0;
    REG_SP = REG_SP + 24;
    ((value_t *)REG_T1)[0] = REG_T2;
    ((value_t *)REG_T1)[1] = REG_T0;
    ((value_t *)REG_T1)[2] = REG_A2;
    REG_T0 = REG_SP + 0;
    REG_SP = REG_SP + 16;
    REG_L0 = (value_t)kCode31;
    ((value_t *)REG_T0)[0] = REG_L0;
    ((value_t *)REG_T0)[1] = REG_T1;
    REG_T1 = REG_ZERO + 0;
    REG_L0 = ((value_t *)REG_S0)[0];
    REG_T2 = ((value_t *)REG_L0)[0];
    REG_L0 = ((value_t *)REG_S0)[0];
    REG_L1 = ((value_t *)REG_L0)[1];
    ((value_t *)REG_S0)[1] = REG_L1;
    REG_L1 = ((value_t *)REG_T0)[0];
    ((value_t *)REG_S0)[2] = REG_L1;
    REG_L1 = ((value_t *)REG_T0)[1];
    ((value_t *)REG_S0)[3] = REG_L1;
    if (REG_A2 == 0)
    {
        GLOBAL_FUNC = (value_t)block57;
    }
    else
    {
        GLOBAL_FUNC = (value_t)block58;
    }
}
void block57()
{
    REG_L0 = ((value_t *)REG_S0)[1];
    REG_A0 = REG_L0 + 0;
    REG_A1 = REG_T1 + 0;
    GLOBAL_FUNC = REG_T2;
}
void block58()
{
    REG_L0 = ((value_t *)REG_S0)[3];
    REG_A0 = REG_L0 + 0;
    REG_A1 = REG_T1 + 0;
    REG_L0 = ((value_t *)REG_S0)[2];
    GLOBAL_FUNC = REG_L0;
}
int main()
{
    GLOBAL_FUNC = (value_t)main56;
    main_loop();
    return 0;
}