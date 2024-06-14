.global main
main:
    la s0 , _stack_bottom
    la sp , _stack_bottom
    j main150
main150:
    mv s0 , sp
    addi sp , sp , 16
    mv t3 , sp
    addi sp , sp , 16
    la t0 , fCode46
    sd t0 , 0(t3)
    sd t2 , 8(t3)
    mv t4 , sp
    addi sp , sp , 16
    la t0 , fCode89
    sd t0 , 0(t4)
    sd t2 , 8(t4)
    addi t2 , zero , 99
    mv t1 , sp
    addi sp , sp , 16
    sd t2 , 0(t1)
    sd t4 , 8(t1)
    sd t1 , 0(s0)
    mv t4 , sp
    addi sp , sp , 16
    la t0 , kCode145
    sd t0 , 0(t4)
    sd t2 , 8(t4)
    ld t2 , 0(t3)
    ld t1 , 8(t3)
    sd t1 , 8(s0)
    ld t0 , 8(s0)
    mv a0 , t0
    mv a1 , t4
    ld t0 , 0(s0)
    mv a2 , t0
    jr t2
kCode66:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    mv a0 , t4
    mv a1 , a1
    jr t3
kCode73:
    mv s0 , sp
    addi sp , sp , 8
    ld t2 , 0(a0)
    ld t3 , 8(a0)
    ld t4 , 0(t2)
    ld t1 , 8(t2)
    sd t1 , 0(s0)
    ld t0 , 0(s0)
    mv a0 , t0
    mv a1 , t3
    jr t4
kCode63:
    mv s0 , sp
    addi sp , sp , 32
    ld t2 , 0(a0)
    ld t1 , 8(a0)
    sd t1 , 0(s0)
    ld t1 , 16(a0)
    sd t1 , 8(s0)
    ld t3 , 24(a0)
    addi t4 , zero , 1
    sub t1 , t3 , t4
    sd t1 , 16(s0)
    mv t3 , sp
    addi sp , sp , 16
    ld t0 , 16(s0)
    sd t0 , 0(t3)
    sd t2 , 8(t3)
    mv t2 , sp
    addi sp , sp , 8
    ld t0 , 8(s0)
    sd t0 , 0(t2)
    mv t4 , sp
    addi sp , sp , 16
    la t0 , kCode66
    sd t0 , 0(t4)
    sd t2 , 8(t4)
    ld t0 , 0(s0)
    ld t2 , 0(t0)
    ld t0 , 0(s0)
    ld t1 , 8(t0)
    sd t1 , 24(s0)
    ld t0 , 24(s0)
    mv a0 , t0
    mv a1 , t4
    mv a2 , t3
    jr t2
kCode58:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    mv a0 , t4
    mv a1 , a1
    jr t3
kCode55:
    mv s0 , sp
    addi sp , sp , 40
    ld t2 , 0(a0)
    ld t1 , 8(a0)
    sd t1 , 8(s0)
    ld t4 , 16(a0)
    ld t1 , 24(a0)
    sd t1 , 0(s0)
    mv t3 , sp
    addi sp , sp , 8
    sd t4 , 0(t3)
    mv t4 , sp
    addi sp , sp , 16
    la t0 , kCode58
    sd t0 , 0(t4)
    sd t3 , 8(t4)
    mv t3 , sp
    addi sp , sp , 32
    sd t2 , 0(t3)
    ld t0 , 8(s0)
    sd t0 , 8(t3)
    sd t4 , 16(t3)
    ld t0 , 0(s0)
    sd t0 , 24(t3)
    mv t2 , sp
    addi sp , sp , 16
    la t0 , kCode63
    sd t0 , 0(t2)
    sd t3 , 8(t2)
    mv t3 , sp
    addi sp , sp , 16
    sd t4 , 0(t3)
    ld t0 , 0(s0)
    sd t0 , 8(t3)
    mv t4 , sp
    addi sp , sp , 16
    la t0 , kCode73
    sd t0 , 0(t4)
    sd t3 , 8(t4)
    mv t3 , zero
    ld t1 , 0(t2)
    sd t1 , 16(s0)
    ld t1 , 8(t2)
    sd t1 , 24(s0)
    ld t2 , 0(t4)
    ld t1 , 8(t4)
    sd t1 , 32(s0)
    beq a1 , zero , block151
    j block152
block151:
    ld t0 , 24(s0)
    mv a0 , t0
    mv a1 , t3
    ld t0 , 16(s0)
    jr t0
block152:
    ld t0 , 32(s0)
    mv a0 , t0
    mv a1 , t3
    jr t2
kCode52:
    mv s0 , sp
    addi sp , sp , 8
    ld t2 , 0(a0)
    ld t3 , 8(a0)
    ld t1 , 16(a0)
    sd t1 , 0(s0)
    mv t4 , sp
    addi sp , sp , 32
    sd a1 , 0(t4)
    sd t2 , 8(t4)
    sd t3 , 16(t4)
    ld t0 , 0(s0)
    sd t0 , 24(t4)
    mv t2 , sp
    addi sp , sp , 16
    la t0 , kCode55
    sd t0 , 0(t2)
    sd t4 , 8(t2)
    ld t3 , 0(a1)
    ld t4 , 8(a1)
    mv a0 , t4
    mv a1 , t2
    ld t0 , 0(s0)
    mv a2 , t0
    jr t3
kCode49:
    mv s0 , sp
    addi sp , sp , 16
    ld t2 , 0(a0)
    ld t3 , 8(a0)
    ld t1 , 16(a0)
    sd t1 , 0(s0)
    mv t4 , sp
    addi sp , sp , 24
    sd t2 , 0(t4)
    sd t3 , 8(t4)
    sd a1 , 16(t4)
    mv t2 , sp
    addi sp , sp , 16
    la t0 , kCode52
    sd t0 , 0(t2)
    sd t4 , 8(t2)
    ld t0 , 0(s0)
    ld t3 , 8(t0)
    ld t4 , 0(t2)
    ld t1 , 8(t2)
    sd t1 , 8(s0)
    ld t0 , 8(s0)
    mv a0 , t0
    mv a1 , t3
    jr t4
kCode121:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    mv a0 , t4
    mv a1 , a1
    jr t3
kCode118:
    mv s0 , sp
    addi sp , sp , 16
    ld t1 , 0(a0)
    sd t1 , 0(s0)
    ld t3 , 8(a0)
    ld t4 , 16(a0)
    addi t2 , zero , 1
    sub t1 , t3 , t2
    sd t1 , 8(s0)
    mv t2 , sp
    addi sp , sp , 8
    sd t4 , 0(t2)
    mv t3 , sp
    addi sp , sp , 16
    la t0 , kCode121
    sd t0 , 0(t3)
    sd t2 , 8(t3)
    ld t0 , 0(s0)
    ld t2 , 0(t0)
    ld t0 , 0(s0)
    ld t4 , 8(t0)
    mv a0 , t4
    mv a1 , t3
    ld t0 , 8(s0)
    mv a2 , t0
    jr t2
kCode113:
    mv s0 , sp
    addi sp , sp , 8
    ld t2 , 0(a0)
    addi t3 , zero , 1
    ld t4 , 0(t2)
    ld t1 , 8(t2)
    sd t1 , 0(s0)
    ld t0 , 0(s0)
    mv a0 , t0
    mv a1 , t3
    jr t4
kCode108:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    mv a0 , t4
    mv a1 , a1
    jr t3
kCode105:
    mv s0 , sp
    addi sp , sp , 48
    ld t1 , 0(a0)
    sd t1 , 8(s0)
    ld t1 , 8(a0)
    sd t1 , 0(s0)
    ld t4 , 16(a0)
    ld t3 , 24(a0)
    ld t0 , 0(s0)
    ld t1 , 0(s0)
    mul t2 , t0 , t1
    sub t1 , t3 , t2
    sd t1 , 16(s0)
    mv t2 , sp
    addi sp , sp , 8
    sd t4 , 0(t2)
    mv t3 , sp
    addi sp , sp , 16
    la t0 , kCode108
    sd t0 , 0(t3)
    sd t2 , 8(t3)
    mv t2 , sp
    addi sp , sp , 8
    sd t3 , 0(t2)
    mv t4 , sp
    addi sp , sp , 16
    la t0 , kCode113
    sd t0 , 0(t4)
    sd t2 , 8(t4)
    mv t2 , sp
    addi sp , sp , 24
    ld t0 , 8(s0)
    sd t0 , 0(t2)
    ld t0 , 0(s0)
    sd t0 , 8(t2)
    sd t3 , 16(t2)
    mv t3 , sp
    addi sp , sp , 16
    la t0 , kCode118
    sd t0 , 0(t3)
    sd t2 , 8(t3)
    mv t2 , zero
    ld t1 , 0(t4)
    sd t1 , 24(s0)
    ld t1 , 8(t4)
    sd t1 , 32(s0)
    ld t4 , 0(t3)
    ld t1 , 8(t3)
    sd t1 , 40(s0)
    ld t0 , 16(s0)
    beq t0 , zero , block153
    j block154
block153:
    ld t0 , 32(s0)
    mv a0 , t0
    mv a1 , t2
    ld t0 , 24(s0)
    jr t0
block154:
    ld t0 , 40(s0)
    mv a0 , t0
    mv a1 , t2
    jr t4
kCode100:
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
kCode95:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    mv a0 , t4
    mv a1 , a1
    jr t3
kCode138:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    mv a0 , t4
    mv a1 , a1
    jr t3
fCode92:
    mv s0 , sp
    addi sp , sp , 40
    mv t2 , sp
    addi sp , sp , 16
    la t0 , fCode92
    sd t0 , 0(t2)
    sd a0 , 8(t2)
    ld t3 , 0(a0)
    mv t4 , sp
    addi sp , sp , 8
    sd a1 , 0(t4)
    mv t1 , sp
    addi sp , sp , 16
    la t0 , kCode95
    sd t0 , 0(t1)
    sd t4 , 8(t1)
    sd t1 , 0(s0)
    mv t4 , sp
    addi sp , sp , 8
    ld t0 , 0(s0)
    sd t0 , 0(t4)
    mv t1 , sp
    addi sp , sp , 16
    la t0 , kCode100
    sd t0 , 0(t1)
    sd t4 , 8(t1)
    sd t1 , 8(s0)
    mv t4 , sp
    addi sp , sp , 32
    sd t2 , 0(t4)
    sd a2 , 8(t4)
    ld t0 , 0(s0)
    sd t0 , 16(t4)
    sd t3 , 24(t4)
    mv t2 , sp
    addi sp , sp , 16
    la t0 , kCode105
    sd t0 , 0(t2)
    sd t4 , 8(t2)
    mv t3 , zero
    ld t0 , 8(s0)
    ld t4 , 0(t0)
    ld t0 , 8(s0)
    ld t1 , 8(t0)
    sd t1 , 16(s0)
    ld t1 , 0(t2)
    sd t1 , 24(s0)
    ld t1 , 8(t2)
    sd t1 , 32(s0)
    beq a2 , zero , block155
    j block156
block155:
    ld t0 , 16(s0)
    mv a0 , t0
    mv a1 , t3
    jr t4
block156:
    ld t0 , 32(s0)
    mv a0 , t0
    mv a1 , t3
    ld t0 , 24(s0)
    jr t0
kCode145:
    mv a0 , a1
    j halt
fCode89:
    mv s0 , sp
    addi sp , sp , 8
    mv t2 , sp
    addi sp , sp , 16
    la t0 , fCode89
    sd t0 , 0(t2)
    sd a0 , 8(t2)
    mv t2 , sp
    addi sp , sp , 8
    sd a2 , 0(t2)
    mv t3 , sp
    addi sp , sp , 16
    la t0 , fCode92
    sd t0 , 0(t3)
    sd t2 , 8(t3)
    mv t2 , sp
    addi sp , sp , 8
    sd a1 , 0(t2)
    mv t4 , sp
    addi sp , sp , 16
    la t0 , kCode138
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
fCode46:
    mv s0 , sp
    addi sp , sp , 8
    mv t2 , sp
    addi sp , sp , 16
    la t0 , fCode46
    sd t0 , 0(t2)
    sd a0 , 8(t2)
    mv t3 , sp
    addi sp , sp , 24
    sd t2 , 0(t3)
    sd a1 , 8(t3)
    sd a2 , 16(t3)
    mv t2 , sp
    addi sp , sp , 16
    la t0 , kCode49
    sd t0 , 0(t2)
    sd t3 , 8(t2)
    ld t3 , 0(a2)
    ld t4 , 0(t2)
    ld t1 , 8(t2)
    sd t1 , 0(s0)
    ld t0 , 0(s0)
    mv a0 , t0
    mv a1 , t3
    jr t4
