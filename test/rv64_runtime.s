# RISC-V Runtime Library (rv_runtime.s)
# Общие функции для работы с памятью, строками и выводом

.section .data
str_buffer:    .string "01234567012345670123456701234567\n"  
not_implemented_str: .string "Not implemented\n\0"
.equ BUFSIZE, 32

.section .text

.global not_implemented
not_implemented:
    li      a7, 64        # syscall write (64)
    li      a0, 1        # stdout (1)
    la      a1, not_implemented_str
    li      a2, 16       # длина строки
    ecall
    li      a0, 1        # exit code 1
    li      a7, 93       # syscall exit (93)
    ecall
    ret

.global memset
memset:
    mv      t0, a0       
memset_loop:
    beqz    a2, memset_end
    sb      a1, 0(t0)
    addi    t0, t0, 1
    addi    a2, a2, -1
    j       memset_loop
memset_end:
    ret

.global strlen
strlen:
    li      t0, 0       
strlen_loop:
    lb      t1, 0(a0)
    beqz    t1, strlen_end
    addi    a0, a0, 1
    addi    t0, t0, 1
    j       strlen_loop
strlen_end:
    mv      a0, t0
    ret

.global memcpy
memcpy:
    mv      t0, a0      
memcpy_loop:
    beqz    a2, memcpy_end
    lb      t1, 0(a1)
    sb      t1, 0(t0)
    addi    a1, a1, 1
    addi    t0, t0, 1
    addi    a2, a2, -1
    j       memcpy_loop
memcpy_end:
    ret

.global myitoa
myitoa:
    li      t1, 10           
    mv      t2, a0           
    li      a1, 0          
    la      a0, str_buffer
    addi    a0, a0, BUFSIZE   

    bnez    t2, myitoa_loop
    addi    a0, a0, -1
    li      t3, '0'
    sb      t3, 0(a0)
    li      a1, 1
    ret

myitoa_loop:
    beqz    t2, myitoa_end
    remu    t3, t2, t1         
    addi    t3, t3, '0'       
    addi    a0, a0, -1
    sb      t3, 0(a0)
    addi    a1, a1, 1
    divu    t2, t2, t1         
    j       myitoa_loop

myitoa_end:
    ret

.global print_int
print_int:
    addi    sp, sp, -16
    sd      ra, 8(sp)
    sd      a0, 0(sp)

    la      a0, str_buffer
    li      a1, ' '
    li      a2, BUFSIZE
    call    memset

    ld      a0, 0(sp)
    call    myitoa

    li      a7, 64             
    li      a0, 1              
    la      a1, str_buffer   
    li      a2, BUFSIZE       
    ecall

    ld      ra, 8(sp)
    addi    sp, sp, 16
    ret

.global trace_variable
trace_variable:
    addi    sp, sp, -32
    sd      ra, 24(sp)
    sd      a0, 16(sp)     
    sd      a1, 8(sp)         
    sd      a2, 0(sp)        

    la      a0, str_buffer
    li      a1, ' '
    li      a2, BUFSIZE
    call    memset

    ld      a0, 0(sp)
    call    myitoa

    la      a0, str_buffer
    ld      a1, 16(sp)
    ld      a2, 8(sp)
    call    memcpy

    la      t0, str_buffer
    ld      t1, 8(sp)
    add     t0, t0, t1
    li      t2, ':'
    sb      t2, 0(t0)
    li      t2, ' '
    sb      t2, 1(t0)

    li      a7, 64
    li      a0, 1
    la      a1, str_buffer
    li      a2, BUFSIZE
    ecall

    ld      ra, 24(sp)
    addi    sp, sp, 32
    ret
