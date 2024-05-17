#include <stdio.h>
#include "runtime.h"
void main49(value_t z50);
void kCode32(value_t z52);
void kCode29(value_t z54);
void kCode24(value_t z56);
void kCode19(value_t z58);
void kCode44(value_t z60);
void fCode16(value_t z61);
void main49(value_t z50)
{
    value_t env18 = (value_t)allocTuple(0);
    value_t sum = (value_t)allocTuple(2);
    ((value_t *)sum)[0] = (value_t)fCode16;
    ((value_t *)sum)[1] = (value_t)env18;
    value_t x13 = (value_t)10;
    value_t env46 = (value_t)allocTuple(0);
    value_t k14 = (value_t)allocTuple(2);
    ((value_t *)k14)[0] = (value_t)kCode44;
    ((value_t *)k14)[1] = (value_t)env46;
    value_t fCode47 = (value_t)((value_t *)sum)[0];
    value_t env48 = (value_t)((value_t *)sum)[1];
    value_t z51 = (value_t)allocTuple(3);
    ((value_t *)z51)[0] = (value_t)env48;
    ((value_t *)z51)[1] = (value_t)k14;
    ((value_t *)z51)[2] = (value_t)x13;
    ((func_t)fCode47)(z51);
}
void kCode32(value_t z52)
{
    value_t env33 = (value_t)((value_t *)z52)[0];
    value_t x11 = (value_t)((value_t *)z52)[1];
    value_t k4 = (value_t)((value_t *)env33)[0];
    value_t x = (value_t)((value_t *)env33)[1];
    value_t x12 = (value_t)x + x11;
    value_t kCode35 = (value_t)((value_t *)k4)[0];
    value_t env36 = (value_t)((value_t *)k4)[1];
    value_t z53 = (value_t)allocTuple(2);
    ((value_t *)z53)[0] = (value_t)env36;
    ((value_t *)z53)[1] = (value_t)x12;
    ((func_t)kCode35)(z53);
}
void kCode29(value_t z54)
{
    value_t env30 = (value_t)((value_t *)z54)[0];
    value_t x3 = (value_t)((value_t *)z54)[1];
    value_t k4 = (value_t)((value_t *)env30)[0];
    value_t sum = (value_t)((value_t *)env30)[1];
    value_t x = (value_t)((value_t *)env30)[2];
    value_t x8 = (value_t)1;
    value_t x9 = (value_t)x - x8;
    value_t env34 = (value_t)allocTuple(2);
    ((value_t *)env34)[0] = (value_t)k4;
    ((value_t *)env34)[1] = (value_t)x;
    value_t k10 = (value_t)allocTuple(2);
    ((value_t *)k10)[0] = (value_t)kCode32;
    ((value_t *)k10)[1] = (value_t)env34;
    value_t fCode37 = (value_t)((value_t *)sum)[0];
    value_t env38 = (value_t)((value_t *)sum)[1];
    value_t z55 = (value_t)allocTuple(3);
    ((value_t *)z55)[0] = (value_t)env38;
    ((value_t *)z55)[1] = (value_t)k10;
    ((value_t *)z55)[2] = (value_t)x9;
    ((func_t)fCode37)(z55);
}
void kCode24(value_t z56)
{
    value_t env25 = (value_t)((value_t *)z56)[0];
    value_t x2 = (value_t)((value_t *)z56)[1];
    value_t k4 = (value_t)((value_t *)env25)[0];
    value_t x7 = (value_t)0;
    value_t kCode27 = (value_t)((value_t *)k4)[0];
    value_t env28 = (value_t)((value_t *)k4)[1];
    value_t z57 = (value_t)allocTuple(2);
    ((value_t *)z57)[0] = (value_t)env28;
    ((value_t *)z57)[1] = (value_t)x7;
    ((func_t)kCode27)(z57);
}
void kCode19(value_t z58)
{
    value_t env20 = (value_t)((value_t *)z58)[0];
    value_t x1 = (value_t)((value_t *)z58)[1];
    value_t k0 = (value_t)((value_t *)env20)[0];
    value_t kCode22 = (value_t)((value_t *)k0)[0];
    value_t env23 = (value_t)((value_t *)k0)[1];
    value_t z59 = (value_t)allocTuple(2);
    ((value_t *)z59)[0] = (value_t)env23;
    ((value_t *)z59)[1] = (value_t)x1;
    ((func_t)kCode22)(z59);
}
void kCode44(value_t z60)
{
    value_t env45 = (value_t)((value_t *)z60)[0];
    value_t x15 = (value_t)((value_t *)z60)[1];
    halt(x15);
}
void fCode16(value_t z61)
{
    value_t env17 = (value_t)((value_t *)z61)[0];
    value_t k0 = (value_t)((value_t *)z61)[1];
    value_t x = (value_t)((value_t *)z61)[2];
    value_t sum = (value_t)allocTuple(2);
    ((value_t *)sum)[0] = (value_t)fCode16;
    ((value_t *)sum)[1] = (value_t)env17;
    value_t env21 = (value_t)allocTuple(1);
    ((value_t *)env21)[0] = (value_t)k0;
    value_t k4 = (value_t)allocTuple(2);
    ((value_t *)k4)[0] = (value_t)kCode19;
    ((value_t *)k4)[1] = (value_t)env21;
    value_t env26 = (value_t)allocTuple(1);
    ((value_t *)env26)[0] = (value_t)k4;
    value_t k5 = (value_t)allocTuple(2);
    ((value_t *)k5)[0] = (value_t)kCode24;
    ((value_t *)k5)[1] = (value_t)env26;
    value_t env31 = (value_t)allocTuple(3);
    ((value_t *)env31)[0] = (value_t)k4;
    ((value_t *)env31)[1] = (value_t)sum;
    ((value_t *)env31)[2] = (value_t)x;
    value_t k6 = (value_t)allocTuple(2);
    ((value_t *)k6)[0] = (value_t)kCode29;
    ((value_t *)k6)[1] = (value_t)env31;
    value_t x39 = (value_t)NULL;
    value_t kCode40 = (value_t)((value_t *)k5)[0];
    value_t env41 = (value_t)((value_t *)k5)[1];
    value_t kCode42 = (value_t)((value_t *)k6)[0];
    value_t env43 = (value_t)((value_t *)k6)[1];
    if (x == 0)
    {
        value_t z62 = (value_t)allocTuple(2);
        ((value_t *)z62)[0] = (value_t)env41;
        ((value_t *)z62)[1] = (value_t)x39;
        ((func_t)kCode40)(z62);
    }
    else
    {
        value_t z63 = (value_t)allocTuple(2);
        ((value_t *)z63)[0] = (value_t)env43;
        ((value_t *)z63)[1] = (value_t)x39;
        ((func_t)kCode42)(z63);
    }
}
int main()
{
    main49(0);
    return 0;
}