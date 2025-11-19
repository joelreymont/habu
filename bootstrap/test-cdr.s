.section __TEXT,__text
.globl _start
.p2align 4

_start:
    ; Initialize heap before running user code
    bl _heap_init

    ; Call user code
    bl _main

    ; Exit with result from main
    mov x16, #1      ; sys_exit
    svc #0x80

_heap_init:
    ; Save return address
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    ; mmap(NULL, heap_size, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0)
    mov x0, #0              ; addr = NULL
    adrp x1, _heap_size@PAGE
    ldr x1, [x1, _heap_size@PAGEOFF]  ; length = heap_size
    mov x2, #3              ; prot = PROT_READ | PROT_WRITE
    mov w3, #0x1002         ; flags = MAP_PRIVATE | MAP_ANONYMOUS
    mov x4, #-1             ; fd = -1
    mov x5, #0              ; offset = 0
    mov x16, #197           ; sys_mmap
    svc #0x80

    ; Check for error (x0 == -1 or x0 >= -4096)
    mov x1, #-1
    cmp x0, x1
    b.eq _mmap_failed

    ; Store heap_start
    adrp x1, _heap_start@PAGE
    str x0, [x1, _heap_start@PAGEOFF]

    ; Store heap_ptr (initially same as heap_start)
    adrp x1, _heap_ptr@PAGE
    str x0, [x1, _heap_ptr@PAGEOFF]

    ; Calculate and store heap_limit = heap_start + heap_size
    adrp x2, _heap_size@PAGE
    ldr x2, [x2, _heap_size@PAGEOFF]
    add x2, x0, x2
    adrp x1, _heap_limit@PAGE
    str x2, [x1, _heap_limit@PAGEOFF]

    ; Restore and return
    ldp x29, x30, [sp], #16
    ret

_mmap_failed:
    ; mmap failed, exit with code 1
    mov x0, #1
    mov x16, #1      ; sys_exit
    svc #0x80

; Cons cell allocation
; Input: x0 = car (tagged), x1 = cdr (tagged)
; Output: x0 = pointer to cons cell (tagged with 0x1)
.globl _habu_cons
_habu_cons:
    ; Save car and cdr
    mov x19, x0
    mov x20, x1

    ; Load heap_ptr
    adrp x0, _heap_ptr@PAGE
    ldr x0, [x0, _heap_ptr@PAGEOFF]

    ; Check if heap_ptr + 16 <= heap_limit
    add x1, x0, #16
    adrp x2, _heap_limit@PAGE
    ldr x2, [x2, _heap_limit@PAGEOFF]
    cmp x1, x2
    b.hi _heap_overflow

    ; Store car at [heap_ptr]
    str x19, [x0]

    ; Store cdr at [heap_ptr+8]
    str x20, [x0, #8]

    ; Update heap_ptr
    adrp x2, _heap_ptr@PAGE
    str x1, [x2, _heap_ptr@PAGEOFF]

    ; Tag pointer with 0x1 and return
    orr x0, x0, #1
    ret

_heap_overflow:
    ; Heap overflow - exit with code 2
    mov x0, #2
    mov x16, #1
    svc #0x80

; Car accessor
; Input: x0 = cons pointer (tagged)
; Output: x0 = car value
.globl _habu_car
_habu_car:
    ; Untag pointer
    and x0, x0, #-2
    ; Load car
    ldr x0, [x0]
    ret

; Cdr accessor
; Input: x0 = cons pointer (tagged)
; Output: x0 = cdr value
.globl _habu_cdr
_habu_cdr:
    ; Untag pointer
    and x0, x0, #-2
    ; Load cdr
    ldr x0, [x0, #8]
    ret

_main:
    .byte 0x00
    .byte 0x0A
    .byte 0x80
    .byte 0xD2
    .byte 0xE0
    .byte 0x0F
    .byte 0x1F
    .byte 0xF8
    .byte 0x00
    .byte 0x14
    .byte 0x80
    .byte 0xD2
    .byte 0xE1
    .byte 0x03
    .byte 0x00
    .byte 0xAA
    .byte 0xE0
    .byte 0x07
    .byte 0x41
    .byte 0xF8
    bl _habu_cons
    bl _habu_cdr
    .byte 0x30
    .byte 0x00
    .byte 0x80
    .byte 0xD2
    .byte 0x01
    .byte 0x10
    .byte 0x00
    .byte 0xD4

.section __DATA,__data
.p2align 3
.globl _heap_start, _heap_ptr, _heap_limit, _heap_size
_heap_start:  .quad 0
_heap_ptr:    .quad 0
_heap_limit:  .quad 0
_heap_size:   .quad 1048576  ; 1MB initial heap
