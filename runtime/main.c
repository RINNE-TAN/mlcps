#include <stdio.h>
#include "runtime.h"
void main56(value_t arg57);
void kCode37(value_t arg62);
void kCode34(value_t arg64);
void kCode31(value_t arg67);
void kCode26(value_t arg68);
void kCode21(value_t arg69);
void kCode51(value_t arg71);
void fCode18(value_t arg72);
void main56(value_t arg57)
{
    value_t env20 = allocTuple(0);
    value_t sum = allocTuple(2);
    ((value_t *)sum)[0] = (value_t)fCode18;
    ((value_t *)sum)[1] = (value_t)env20;
    value_t x15 = 10;
    value_t env53 = allocTuple(0);
    value_t k16 = allocTuple(2);
    ((value_t *)k16)[0] = (value_t)kCode51;
    ((value_t *)k16)[1] = (value_t)env53;
    value_t fCode54 = ((value_t *)sum)[0];
    value_t env55 = ((value_t *)sum)[1];
    value_t arg70 = allocTuple(3);
    ((value_t *)arg70)[0] = (value_t)env55;
    ((value_t *)arg70)[1] = (value_t)k16;
    ((value_t *)arg70)[2] = (value_t)x15;
    GLOBAL_FUNC = fCode54;
    GLOBAL_ARG = arg70;
}
void kCode37(value_t arg62)
{
    value_t env38 = ((value_t *)arg62)[0];
    value_t x12 = ((value_t *)arg62)[1];
    value_t k4 = ((value_t *)env38)[0];
    value_t x = ((value_t *)env38)[1];
    value_t x13 = x + x12;
    value_t kCode40 = ((value_t *)k4)[0];
    value_t env41 = ((value_t *)k4)[1];
    value_t arg60 = allocTuple(2);
    ((value_t *)arg60)[0] = (value_t)env41;
    ((value_t *)arg60)[1] = (value_t)x13;
    GLOBAL_FUNC = kCode40;
    GLOBAL_ARG = arg60;
}
void kCode34(value_t arg64)
{
    value_t env35 = ((value_t *)arg64)[0];
    value_t _ = ((value_t *)arg64)[1];
    value_t k4 = ((value_t *)env35)[0];
    value_t sum = ((value_t *)env35)[1];
    value_t x = ((value_t *)env35)[2];
    value_t x9 = 1;
    value_t x10 = x - x9;
    value_t env39 = allocTuple(2);
    ((value_t *)env39)[0] = (value_t)k4;
    ((value_t *)env39)[1] = (value_t)x;
    value_t k11 = allocTuple(2);
    ((value_t *)k11)[0] = (value_t)kCode37;
    ((value_t *)k11)[1] = (value_t)env39;
    value_t fCode42 = ((value_t *)sum)[0];
    value_t env43 = ((value_t *)sum)[1];
    value_t arg61 = allocTuple(3);
    ((value_t *)arg61)[0] = (value_t)env43;
    ((value_t *)arg61)[1] = (value_t)k11;
    ((value_t *)arg61)[2] = (value_t)x10;
    GLOBAL_FUNC = fCode42;
    GLOBAL_ARG = arg61;
}
void kCode31(value_t arg67)
{
    value_t env32 = ((value_t *)arg67)[0];
    value_t x3 = ((value_t *)arg67)[1];
    value_t k4 = ((value_t *)env32)[0];
    value_t sum = ((value_t *)env32)[1];
    value_t x = ((value_t *)env32)[2];
    value_t env36 = allocTuple(3);
    ((value_t *)env36)[0] = (value_t)k4;
    ((value_t *)env36)[1] = (value_t)sum;
    ((value_t *)env36)[2] = (value_t)x;
    value_t j8 = allocTuple(2);
    ((value_t *)j8)[0] = (value_t)kCode34;
    ((value_t *)j8)[1] = (value_t)env36;
    value_t x14 = print(x);
    value_t kCode44 = ((value_t *)j8)[0];
    value_t env45 = ((value_t *)j8)[1];
    value_t arg63 = allocTuple(2);
    ((value_t *)arg63)[0] = (value_t)env45;
    ((value_t *)arg63)[1] = (value_t)x14;
    GLOBAL_FUNC = kCode44;
    GLOBAL_ARG = arg63;
}
void kCode26(value_t arg68)
{
    value_t env27 = ((value_t *)arg68)[0];
    value_t x2 = ((value_t *)arg68)[1];
    value_t k4 = ((value_t *)env27)[0];
    value_t x7 = 0;
    value_t kCode29 = ((value_t *)k4)[0];
    value_t env30 = ((value_t *)k4)[1];
    value_t arg59 = allocTuple(2);
    ((value_t *)arg59)[0] = (value_t)env30;
    ((value_t *)arg59)[1] = (value_t)x7;
    GLOBAL_FUNC = kCode29;
    GLOBAL_ARG = arg59;
}
void kCode21(value_t arg69)
{
    value_t env22 = ((value_t *)arg69)[0];
    value_t x1 = ((value_t *)arg69)[1];
    value_t k0 = ((value_t *)env22)[0];
    value_t kCode24 = ((value_t *)k0)[0];
    value_t env25 = ((value_t *)k0)[1];
    value_t arg58 = allocTuple(2);
    ((value_t *)arg58)[0] = (value_t)env25;
    ((value_t *)arg58)[1] = (value_t)x1;
    GLOBAL_FUNC = kCode24;
    GLOBAL_ARG = arg58;
}
void kCode51(value_t arg71)
{
    value_t env52 = ((value_t *)arg71)[0];
    value_t x17 = ((value_t *)arg71)[1];
    GLOBAL_FUNC = (value_t)halt;
    GLOBAL_ARG = x17;
}
void fCode18(value_t arg72)
{
    value_t env19 = ((value_t *)arg72)[0];
    value_t k0 = ((value_t *)arg72)[1];
    value_t x = ((value_t *)arg72)[2];
    value_t sum = allocTuple(2);
    ((value_t *)sum)[0] = (value_t)fCode18;
    ((value_t *)sum)[1] = (value_t)env19;
    value_t env23 = allocTuple(1);
    ((value_t *)env23)[0] = (value_t)k0;
    value_t k4 = allocTuple(2);
    ((value_t *)k4)[0] = (value_t)kCode21;
    ((value_t *)k4)[1] = (value_t)env23;
    value_t env28 = allocTuple(1);
    ((value_t *)env28)[0] = (value_t)k4;
    value_t k5 = allocTuple(2);
    ((value_t *)k5)[0] = (value_t)kCode26;
    ((value_t *)k5)[1] = (value_t)env28;
    value_t env33 = allocTuple(3);
    ((value_t *)env33)[0] = (value_t)k4;
    ((value_t *)env33)[1] = (value_t)sum;
    ((value_t *)env33)[2] = (value_t)x;
    value_t k6 = allocTuple(2);
    ((value_t *)k6)[0] = (value_t)kCode31;
    ((value_t *)k6)[1] = (value_t)env33;
    value_t x46 = (value_t)NULL;
    value_t kCode47 = ((value_t *)k5)[0];
    value_t env48 = ((value_t *)k5)[1];
    value_t arg65 = allocTuple(2);
    ((value_t *)arg65)[0] = (value_t)env48;
    ((value_t *)arg65)[1] = (value_t)x46;
    value_t kCode49 = ((value_t *)k6)[0];
    value_t env50 = ((value_t *)k6)[1];
    value_t arg66 = allocTuple(2);
    ((value_t *)arg66)[0] = (value_t)env50;
    ((value_t *)arg66)[1] = (value_t)x46;
    if (x == 0)
    {
        GLOBAL_FUNC = kCode47;
        GLOBAL_ARG = arg65;
    }
    else
    {
        GLOBAL_FUNC = kCode49;
        GLOBAL_ARG = arg66;
    }
}
int main()
{
    GLOBAL_FUNC = (value_t)main56;
    main_loop();
    return 0;
}