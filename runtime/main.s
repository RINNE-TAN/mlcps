.global main
main:
    la s0 , _stack_bottom
    la sp , _stack_bottom
    j main49
main49:
    mv s0 , sp
    addi sp , sp , 16
    mv t3 , sp
    addi sp , sp , 16
    la t0 , fCode16
    sd t0 , 0(t3)
    sd t2 , 8(t3)
    addi t2 , zero , 10
    mv t1 , sp
    addi sp , sp , 16
    la t0 , kCode44
    sd t0 , 0(t1)
    sd t4 , 8(t1)
    sd t1 , 0(s0)
    ld t4 , 0(t3)
    ld t1 , 8(t3)
    sd t1 , 8(s0)
    ld t0 , 8(s0)
    mv a0 , t0
    ld t0 , 0(s0)
    mv a1 , t0
    mv a2 , t2
    jr t4
kCode32:
    mv s0 , sp
    addi sp , sp , 8
    ld t2 , 0(a0)
    ld t3 , 8(a0)
    add t4 , t3 , a1
    ld t3 , 0(t2)
    ld t1 , 8(t2)
    sd t1 , 0(s0)
    ld t0 , 0(s0)
    mv a0 , t0
    mv a1 , t4
    jr t3
kCode29:
    mv s0 , sp
    addi sp , sp , 16
    ld t2 , 0(a0)
    ld t1 , 8(a0)
    sd t1 , 0(s0)
    ld t4 , 16(a0)
    addi t3 , zero , 1
    sub t1 , t4 , t3
    sd t1 , 8(s0)
    mv t3 , sp
    addi sp , sp , 16
    sd t2 , 0(t3)
    sd t4 , 8(t3)
    mv t2 , sp
    addi sp , sp , 16
    la t0 , kCode32
    sd t0 , 0(t2)
    sd t3 , 8(t2)
    ld t0 , 0(s0)
    ld t3 , 0(t0)
    ld t0 , 0(s0)
    ld t4 , 8(t0)
    mv a0 , t4
    mv a1 , t2
    ld t0 , 8(s0)
    mv a2 , t0
    jr t3
kCode24:
    mv s0 , sp
    addi sp , sp , 8
    ld t2 , 0(a0)
    addi t3 , zero , 0
    ld t4 , 0(t2)
    ld t1 , 8(t2)
    sd t1 , 0(s0)
    ld t0 , 0(s0)
    mv a0 , t0
    mv a1 , t3
    jr t4
kCode19:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    mv a0 , t4
    mv a1 , a1
    jr t3
kCode44:
    mv a0 , a1
    j halt
fCode16:
    mv s0 , sp
    addi sp , sp , 32
    mv t2 , sp
    addi sp , sp , 16
    la t0 , fCode16
    sd t0 , 0(t2)
    sd a0 , 8(t2)
    mv t3 , sp
    addi sp , sp , 8
    sd a1 , 0(t3)
    mv t4 , sp
    addi sp , sp , 16
    la t0 , kCode19
    sd t0 , 0(t4)
    sd t3 , 8(t4)
    mv t3 , sp
    addi sp , sp , 8
    sd t4 , 0(t3)
    mv t1 , sp
    addi sp , sp , 16
    la t0 , kCode24
    sd t0 , 0(t1)
    sd t3 , 8(t1)
    sd t1 , 0(s0)
    mv t3 , sp
    addi sp , sp , 24
    sd t4 , 0(t3)
    sd t2 , 8(t3)
    sd a2 , 16(t3)
    mv t2 , sp
    addi sp , sp , 16
    la t0 , kCode29
    sd t0 , 0(t2)
    sd t3 , 8(t2)
    mv t3 , zero
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
    mv a0 , t0
    mv a1 , t3
    jr t4
block51:
    ld t0 , 24(s0)
    mv a0 , t0
    mv a1 , t3
    ld t0 , 16(s0)
    jr t0
