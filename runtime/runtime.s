halt:
    addi a1 , a0 , 0
    la   a0 , halt_format
    call printf
    addi a0 , zero , 0
    addi a7 , x0 , 93
    ecall
