.section .data
    halt_format: .asciz "halt with result: %ld\n"
.section .bss
    .align 4                
    .skip 4096               
_stack_bottom:
    .space 4096             
_stack_top:
