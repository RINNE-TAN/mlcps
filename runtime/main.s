.global main
main:
    la s0 , _stack_bottom
    la sp , _stack_bottom
    j main134
main134:
    mv s0 , sp
    addi sp , sp , 24
    mv t3 , sp
    addi sp , sp , 16
    la t0 , fCode40
    sd t0 , 0(t3)
    sd t2 , 8(t3)
    mv t4 , sp
    addi sp , sp , 16
    la t0 , fCode73
    sd t0 , 0(t4)
    sd t2 , 8(t4)
    addi t1 , zero , 99
    sd t1 , 0(s0)
    mv t1 , sp
    addi sp , sp , 16
    la t0 , kCode129
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
kCode54:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    mv a0 , t4
    mv a1 , a1
    jr t3
kCode61:
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
kCode51:
    mv s0 , sp
    addi sp , sp , 24
    ld t1 , 0(a0)
    sd t1 , 0(s0)
    ld t1 , 8(a0)
    sd t1 , 8(s0)
    ld t4 , 16(a0)
    ld t2 , 24(a0)
    addi t3 , zero , 1
    sub t1 , t2 , t3
    sd t1 , 16(s0)
    mv t2 , sp
    addi sp , sp , 8
    sd t4 , 0(t2)
    mv t3 , sp
    addi sp , sp , 16
    la t0 , kCode54
    sd t0 , 0(t3)
    sd t2 , 8(t3)
    ld t0 , 8(s0)
    ld t2 , 0(t0)
    ld t0 , 8(s0)
    ld t4 , 8(t0)
    mv a0 , t4
    mv a1 , t3
    ld t0 , 16(s0)
    mv a2 , t0
    ld t0 , 0(s0)
    mv a3 , t0
    jr t2
kCode46:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    mv a0 , t4
    mv a1 , a1
    jr t3
kCode43:
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
    la t0 , kCode46
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
    la t0 , kCode51
    sd t0 , 0(t2)
    sd t3 , 8(t2)
    mv t3 , sp
    addi sp , sp , 16
    sd t4 , 0(t3)
    ld t0 , 0(s0)
    sd t0 , 8(t3)
    mv t4 , sp
    addi sp , sp , 16
    la t0 , kCode61
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
    beq a1 , zero , block135
    j block136
block135:
    ld t0 , 24(s0)
    mv a0 , t0
    mv a1 , t3
    ld t0 , 16(s0)
    jr t0
block136:
    ld t0 , 32(s0)
    mv a0 , t0
    mv a1 , t3
    jr t2
kCode105:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    mv a0 , t4
    mv a1 , a1
    jr t3
kCode102:
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
    la t0 , kCode105
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
kCode97:
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
kCode92:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    mv a0 , t4
    mv a1 , a1
    jr t3
kCode89:
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
    la t0 , kCode92
    sd t0 , 0(t3)
    sd t2 , 8(t3)
    mv t2 , sp
    addi sp , sp , 8
    sd t3 , 0(t2)
    mv t4 , sp
    addi sp , sp , 16
    la t0 , kCode97
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
    la t0 , kCode102
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
    beq t0 , zero , block137
    j block138
block137:
    ld t0 , 32(s0)
    mv a0 , t0
    mv a1 , t2
    ld t0 , 24(s0)
    jr t0
block138:
    ld t0 , 40(s0)
    mv a0 , t0
    mv a1 , t2
    jr t4
kCode84:
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
kCode79:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    mv a0 , t4
    mv a1 , a1
    jr t3
kCode122:
    ld t2 , 0(a0)
    ld t3 , 0(t2)
    ld t4 , 8(t2)
    mv a0 , t4
    mv a1 , a1
    jr t3
fCode76:
    mv s0 , sp
    addi sp , sp , 40
    mv t2 , sp
    addi sp , sp , 16
    la t0 , fCode76
    sd t0 , 0(t2)
    sd a0 , 8(t2)
    ld t3 , 0(a0)
    mv t4 , sp
    addi sp , sp , 8
    sd a1 , 0(t4)
    mv t1 , sp
    addi sp , sp , 16
    la t0 , kCode79
    sd t0 , 0(t1)
    sd t4 , 8(t1)
    sd t1 , 0(s0)
    mv t4 , sp
    addi sp , sp , 8
    ld t0 , 0(s0)
    sd t0 , 0(t4)
    mv t1 , sp
    addi sp , sp , 16
    la t0 , kCode84
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
    la t0 , kCode89
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
    beq a2 , zero , block139
    j block140
block139:
    ld t0 , 16(s0)
    mv a0 , t0
    mv a1 , t3
    jr t4
block140:
    ld t0 , 32(s0)
    mv a0 , t0
    mv a1 , t3
    ld t0 , 24(s0)
    jr t0
kCode129:
    mv a0 , a1
    j halt
fCode73:
    mv s0 , sp
    addi sp , sp , 8
    mv t2 , sp
    addi sp , sp , 16
    la t0 , fCode73
    sd t0 , 0(t2)
    sd a0 , 8(t2)
    mv t2 , sp
    addi sp , sp , 8
    sd a2 , 0(t2)
    mv t3 , sp
    addi sp , sp , 16
    la t0 , fCode76
    sd t0 , 0(t3)
    sd t2 , 8(t3)
    mv t2 , sp
    addi sp , sp , 8
    sd a1 , 0(t2)
    mv t4 , sp
    addi sp , sp , 16
    la t0 , kCode122
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
fCode40:
    mv t2 , sp
    addi sp , sp , 16
    la t0 , fCode40
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
    la t0 , kCode43
    sd t0 , 0(t2)
    sd t3 , 8(t2)
    ld t3 , 0(a3)
    ld t4 , 8(a3)
    mv a0 , t4
    mv a1 , t2
    mv a2 , a2
    jr t3
