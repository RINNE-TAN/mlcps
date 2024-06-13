.global main
main:
    la s0 , _stack_bottom
    la sp , _stack_bottom
    j main49
main49:
    addi s0 , sp , 0
    addi sp , sp , 16
    addi t3 , sp , 0
    addi sp , sp , 16
    la t0 , fCode16
    sd t0 , 0(t3)
    sd t2 , 8(t3)
    addi t2 , zero , 10
    addi t1 , sp , 0
    addi sp , sp , 16
    la t0 , kCode44
    sd t0 , 0(t1)
    sd t4 , 8(t1)
    sd t1 , 0(s0)
    ld t4 , 0(t3)
    ld t1 , 8(t3)
    sd t1 , 8(s0)
    ld t0 , 8(s0)
    addi a0 , t0 , 0
    ld t0 , 0(s0)
    addi a1 , t0 , 0
    addi a2 , t2 , 0
    jr t4
kCode32:
    addi s0 , sp , 0
    addi sp , sp , 8
    ld t2 , 0(a0)
    ld t3 , 8(a0)
    add t4 , t3 , a1
    ld t3 , 0(t2)
    ld t1 , 8(t2)
    sd t1 , 0(s0)
    ld t0 , 0(s0)
    addi a0 , t0 , 0
    addi a1 , t4 , 0
    jr t3
kCode29:
    addi s0 , sp , 0
    addi sp , sp , 16
    ld t2 , 0(a0)
    ld t1 , 8(a0)
    sd t1 , 0(s0)
    ld t4 , 16(a0)
    addi t3 , zero , 1
    sub t1 , t4 , t3
    sd t1 , 8(s0)
    addi t3 , sp , 0
    addi sp , sp , 16
    sd t2 , 0(t3)
    sd t4 , 8(t3)
    addi t2 , sp , 0
    addi sp , sp , 16
    la t0 , kCode32
    sd t0 , 0(t2)
    sd t3 , 8(t2)
    ld t0 , 0(s0)
    ld t3 , 0(t0)
    ld t0 , 0(s0)
    ld t4 , 8(t0)
    addi a0 , t4 , 0
    addi a1 , t2 , 0
    ld t0 , 8(s0)
    addi a2 , t0 , 0
    jr t3
kCode24:
    addi s0 , sp , 0
    addi sp , sp , 8
    ld t2 , 0(a0)
    addi t3 , zero , 0
    ld t4 , 0(t2)
    ld t1 , 8(t2)
    sd t1 , 0(s0)
    ld t0 , 0(s0)
    addi a0 , t0 , 0
    addi a1 , t3 , 0
    jr t4
kCode19:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    addi a0 , t4 , 0
    addi a1 , a1 , 0
    jr t3
kCode44:
    addi a0 , a1 , 0
    j halt
fCode16:
    addi s0 , sp , 0
    addi sp , sp , 32
    addi t2 , sp , 0
    addi sp , sp , 16
    la t0 , fCode16
    sd t0 , 0(t2)
    sd a0 , 8(t2)
    addi t3 , sp , 0
    addi sp , sp , 8
    sd a1 , 0(t3)
    addi t4 , sp , 0
    addi sp , sp , 16
    la t0 , kCode19
    sd t0 , 0(t4)
    sd t3 , 8(t4)
    addi t3 , sp , 0
    addi sp , sp , 8
    sd t4 , 0(t3)
    addi t1 , sp , 0
    addi sp , sp , 16
    la t0 , kCode24
    sd t0 , 0(t1)
    sd t3 , 8(t1)
    sd t1 , 0(s0)
    addi t3 , sp , 0
    addi sp , sp , 24
    sd t4 , 0(t3)
    sd t2 , 8(t3)
    sd a2 , 16(t3)
    addi t2 , sp , 0
    addi sp , sp , 16
    la t0 , kCode29
    sd t0 , 0(t2)
    sd t3 , 8(t2)
    addi t3 , zero , 0
    ld t0 , 0(s0)
    ld t4 , 0(t0)
    ld t0 , 0(s0)
    ld t1 , 8(t0)
    sd t1 , 8(s0)
    ld t1 , 0(t2)
    sd t1 , 16(s0)
    ld t1 , 8(t2)
    sd t1 , 24(s0)
    beq a2 , zero , block50
    j block51
block50:
    ld t0 , 8(s0)
    addi a0 , t0 , 0
    addi a1 , t3 , 0
    jr t4
block51:
    ld t0 , 24(s0)
    addi a0 , t0 , 0
    addi a1 , t3 , 0
    ld t0 , 16(s0)
    jr t0
