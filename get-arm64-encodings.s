.section __TEXT,__text
.globl _main
.p2align 2

_main:
    // Test all our needed instructions
    movz x0, #672       // Load immediate
    movz x1, #100       // Load to x1
    movz x2, #200       // Load to x2
    
    add x0, x0, x1      // Add
    sub x0, x0, x1      // Subtract  
    mul x0, x0, x1      // Multiply
    
    lsl x0, x0, #4      // Left shift 4
    lsr x0, x0, #4      // Right shift 4
    
    str x0, [sp, #-16]! // Store pre-decrement
    ldr x0, [sp], #16   // Load post-increment
    
    stp x29, x30, [sp, #-16]!  // Store pair
    ldp x29, x30, [sp], #16    // Load pair
    
    mov x1, x0          // Move x0 to x1
    mov x2, x0          // Move x0 to x2
    mov sp, x29         // Move x29 to sp
    mov x29, sp         // Move sp to x29
    
    ret                 // Return
