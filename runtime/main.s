.global main
main:
    la s0 , _stack_bottom
    la sp , _stack_bottom
    j main68
main68:
    mv s0 , sp
    addi sp , sp , 24
    mv t3 , sp
    addi sp , sp , 16
    la t0 , fCode22
    sd t0 , 0(t3)
    sd t2 , 8(t3)
    mv t4 , sp
    addi sp , sp , 16
    la t0 , fCode39
    sd t0 , 0(t4)
    sd t2 , 8(t4)
    addi t1 , zero , 99
    sd t1 , 0(s0)
    mv t1 , sp
    addi sp , sp , 16
    la t0 , kCode63
    sd t0 , 0(t1)
    sd t2 , 8(t1)
    sd t1 , 8(s0)
    ld t2 , 0(t3)
    ld t1 , 8(t3)
    sd t1 , 16(s0)
    ld t0 , 16(s0)
    mv a0 , t0
    ld t0 , 8(s0)
    mv a1 , t0
    ld t0 , 0(s0)
    mv a2 , t0
    mv a3 , t4
    jr t2
kCode28:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    mv a0 , t4
    mv a1 , a1
    jr t3
kCode25:
    mv s0 , sp
    addi sp , sp , 48
    ld t1 , 0(a0)
    sd t1 , 8(s0)
    ld t3 , 8(a0)
    ld t4 , 16(a0)
    ld t1 , 24(a0)
    sd t1 , 0(s0)
    addi t2 , zero , 1
    ld t0 , 0(s0)
    sub t1 , t0 , t2
    sd t1 , 16(s0)
    mv t2 , sp
    addi sp , sp , 8
    sd t4 , 0(t2)
    mv t1 , sp
    addi sp , sp , 16
    la t0 , kCode28
    sd t0 , 0(t1)
    sd t2 , 8(t1)
    sd t1 , 24(s0)
    ld t2 , 0(t3)
    ld t1 , 8(t3)
    sd t1 , 32(s0)
    ld t3 , 0(t4)
    ld t1 , 8(t4)
    sd t1 , 40(s0)
    beq a1 , zero , block69
    j block70
block69:
    ld t0 , 32(s0)
    mv a0 , t0
    ld t0 , 24(s0)
    mv a1 , t0
    ld t0 , 16(s0)
    mv a2 , t0
    ld t0 , 8(s0)
    mv a3 , t0
    jr t2
block70:
    ld t0 , 40(s0)
    mv a0 , t0
    ld t0 , 0(s0)
    mv a1 , t0
    jr t3
kCode49:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    mv a0 , t4
    mv a1 , a1
    jr t3
kCode56:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    mv a0 , t4
    mv a1 , a1
    jr t3
fCode42:
    mv s0 , sp
    addi sp , sp , 72
    mv t2 , sp
    addi sp , sp , 16
    la t0 , fCode42
    sd t0 , 0(t2)
    sd a0 , 8(t2)
    ld t3 , 0(a0)
    addi t1 , zero , 0
    sd t1 , 16(s0)
    ld t1 , 0(a1)
    sd t1 , 0(s0)
    ld t1 , 8(a1)
    sd t1 , 8(s0)
    mul t4 , a2 , a2
    sub t1 , t3 , t4
    sd t1 , 24(s0)
    addi t3 , zero , 1
    ld t1 , 0(a1)
    sd t1 , 40(s0)
    ld t1 , 8(a1)
    sd t1 , 32(s0)
    addi t4 , zero , 1
    sub t1 , a2 , t4
    sd t1 , 48(s0)
    mv t4 , sp
    addi sp , sp , 8
    sd a1 , 0(t4)
    mv t1 , sp
    addi sp , sp , 16
    la t0 , kCode49
    sd t0 , 0(t1)
    sd t4 , 8(t1)
    sd t1 , 56(s0)
    ld t4 , 0(t2)
    ld t1 , 8(t2)
    sd t1 , 64(s0)
    beq a2 , zero , block71
    j block72
block71:
    ld t0 , 8(s0)
    mv a0 , t0
    ld t0 , 16(s0)
    mv a1 , t0
    ld t0 , 0(s0)
    jr t0
block72:
    ld t0 , 24(s0)
    beq t0 , zero , block73
    j block74
block73:
    ld t0 , 32(s0)
    mv a0 , t0
    mv a1 , t3
    ld t0 , 40(s0)
    jr t0
block74:
    ld t0 , 64(s0)
    mv a0 , t0
    ld t0 , 56(s0)
    mv a1 , t0
    ld t0 , 48(s0)
    mv a2 , t0
    jr t4
kCode63:
    mv a0 , a1
    j halt
fCode39:
    mv s0 , sp
    addi sp , sp , 8
    mv t2 , sp
    addi sp , sp , 16
    la t0 , fCode39
    sd t0 , 0(t2)
    sd a0 , 8(t2)
    mv t2 , sp
    addi sp , sp , 8
    sd a2 , 0(t2)
    mv t3 , sp
    addi sp , sp , 16
    la t0 , fCode42
    sd t0 , 0(t3)
    sd t2 , 8(t3)
    mv t2 , sp
    addi sp , sp , 8
    sd a1 , 0(t2)
    mv t4 , sp
    addi sp , sp , 16
    la t0 , kCode56
    sd t0 , 0(t4)
    sd t2 , 8(t4)
    ld t2 , 0(t3)
    ld t1 , 8(t3)
    sd t1 , 0(s0)
    ld t0 , 0(s0)
    mv a0 , t0
    mv a1 , t4
    mv a2 , a2
    jr t2
fCode22:
    mv t2 , sp
    addi sp , sp , 16
    la t0 , fCode22
    sd t0 , 0(t2)
    sd a0 , 8(t2)
    mv t3 , sp
    addi sp , sp , 32
    sd a3 , 0(t3)
    sd t2 , 8(t3)
    sd a1 , 16(t3)
    sd a2 , 24(t3)
    mv t2 , sp
    addi sp , sp , 16
    la t0 , kCode25
    sd t0 , 0(t2)
    sd t3 , 8(t2)
    ld t3 , 0(a3)
    ld t4 , 8(a3)
    mv a0 , t4
    mv a1 , t2
    mv a2 , a2
    jr t3
