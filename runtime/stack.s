.section .data
    halt_format: .asciz "halt with result: %ld\n"
.section .bss
    .align 4                
    .skip 1048576               
_stack_bottom:
    .space 1048576             
_stack_top:
