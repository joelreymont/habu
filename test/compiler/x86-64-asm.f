\ x86-64-asm.f - the x86_64 encoder suite (package X64ASM,
\ src/arch/x86-64/asm.f). Each expected byte string came from
\ `llvm-mc -triple=x86_64 -show-encoding` (LLVM 22.1.8) on the source line in
\ the `llvm-mc:` comment above it; relative branches were read with
\ `llvm-objdump -d` from an `llvm-mc -filetype=obj` object.

require src/lib/test/assert.f
require src/arch/x86-64/asm.f
require src/lib/fmath.f                       \ FMATH:E-DOMAIN, borrowed for a malformed expected string
require src/lib/heap.f
require src/lib/span.f
require src/lib/vector.f

package X64ASM-TEST
private

\ One sink for the whole suite: each case encodes into it, is compared, and
\ clears it, so a case can never read a neighbour's bytes.
TYPED-VARIABLE SINK-CELL ^VEC:vector<u8>
: SINK ( -- ^VEC:vector<u8> ) SINK-CELL @ ;

: SINK-LEN ( -- n ) SINK VEC:COUNT ;

\ A malformed expected string is a defect in this suite's own data, so it dies
\ with FMATH:E-DOMAIN, the tree's "input outside this word's domain" code.
: HEX-DIGIT ( n -- n ) {: c:n :}
   c 48 >= c 57 <= and if c 48 - exit then
   c 97 >= c 102 <= and 0= if FMATH:E-DOMAIN throw then
   c 97 - 10 + ;

: HEX-BYTE ( ^u8 n -- n ) {: a:^ i:n :}
   a i 2 * + c@ HEX-DIGIT 4 lshift
   a i 2 * 1 + + c@ HEX-DIGIT or ;

: SPAN=HEX? ( str str -- bool ) {: d:str e:str :}
   e STR:LEN 2 mod 0<> if FMATH:E-DOMAIN throw then
   d STR:LEN e STR:LEN 2 / <> if false exit then
   d STR:LEN 0 ?do
      e STR:DATA i HEX-BYTE d STR:DATA i + c@ <>
      if false unloop exit then
   loop
   true ;

: X= ( str -- ) {: e:str :}
   SINK VEC:BYTES e SPAN=HEX? TTRUE
   SINK 0 VEC:RESIZE ;

\ asm.f has one encoding per operation, so cases avoid rax immediates and shift
\ counts of one, where llvm-mc picks the short forms (05 id, A9 id, 90+r,
\ D1 /n).
\ add and or in all five forms: register to register, register
\ from memory, memory from register, sign-extended imm8 and imm32.
: ALU-ADD-OR ( -- )
   \ llvm-mc: addq %rbx, %rax
   X64ASM:RAX X64ASM:RBX SINK X64ASM:ENC-ADD-RR   s" 4801d8" X=
   \ llvm-mc: addq (%rcx), %rax
   X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT SINK X64ASM:ENC-ADD-RM   s" 480301" X=
   \ llvm-mc: addq %rax, (%rcx)
   X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT SINK X64ASM:ENC-ADD-MR   s" 480101" X=
   \ llvm-mc: addq $7, %rax
   X64ASM:RAX 7 X64ASM:>IMM8 SINK X64ASM:ENC-ADD-RI8   s" 4883c007" X=
   X64ASM:RBX 72 X64ASM:MEM-OFF -16 X64ASM:>IMM8 SINK X64ASM:ENC-ADD-MI8   s" 48834348f0" X=
   \ llvm-mc: addq $1000, %rcx
   X64ASM:RCX 1000 X64ASM:>IMM32 SINK X64ASM:ENC-ADD-RI32   s" 4881c1e8030000" X=
   \ llvm-mc: orq %r15, %r8
   X64ASM:R8 X64ASM:R15 SINK X64ASM:ENC-OR-RR   s" 4d09f8" X=
   \ llvm-mc: orq 16(%rsi), %rdx
   X64ASM:RDX X64ASM:RSI 16 X64ASM:MEM-OFF SINK X64ASM:ENC-OR-RM   s" 480b5610" X=
   \ llvm-mc: orq %rdx, 16(%rsi)
   X64ASM:RDX X64ASM:RSI 16 X64ASM:MEM-OFF SINK X64ASM:ENC-OR-MR   s" 48095610" X=
   \ llvm-mc: orq $-1, %rbx
   X64ASM:RBX -1 X64ASM:>IMM8 SINK X64ASM:ENC-OR-RI8   s" 4883cbff" X=
   \ llvm-mc: orq $2147483647, %rbx
   X64ASM:RBX 2147483647 X64ASM:>IMM32 SINK X64ASM:ENC-OR-RI32   s" 4881cbffffff7f" X= ;

\ adc and sbb. The r12 base needs a SIB byte with no index and
\ the r13 base needs a disp8 of zero; both also set REX.B.
: ALU-ADC-SBB ( -- )
   \ llvm-mc: adcq %rax, %rdx
   X64ASM:RDX X64ASM:RAX SINK X64ASM:ENC-ADC-RR   s" 4811c2" X=
   \ llvm-mc: adcq (%r12), %r9
   X64ASM:R9 X64ASM:R12 X64ASM:MEM-AT SINK X64ASM:ENC-ADC-RM   s" 4d130c24" X=
   \ llvm-mc: adcq %r9, (%r12)
   X64ASM:R9 X64ASM:R12 X64ASM:MEM-AT SINK X64ASM:ENC-ADC-MR   s" 4d110c24" X=
   \ llvm-mc: adcq $0, %r10
   X64ASM:R10 0 X64ASM:>IMM8 SINK X64ASM:ENC-ADC-RI8   s" 4983d200" X=
   \ llvm-mc: adcq $-2147483648, %r10
   X64ASM:R10 -2147483648 X64ASM:>IMM32 SINK X64ASM:ENC-ADC-RI32   s" 4981d200000080" X=
   \ llvm-mc: sbbq %rcx, %rdx
   X64ASM:RDX X64ASM:RCX SINK X64ASM:ENC-SBB-RR   s" 4819ca" X=
   \ llvm-mc: sbbq (%r13), %rbp
   X64ASM:RBP X64ASM:R13 X64ASM:MEM-AT SINK X64ASM:ENC-SBB-RM   s" 491b6d00" X=
   \ llvm-mc: sbbq %rbp, (%r13)
   X64ASM:RBP X64ASM:R13 X64ASM:MEM-AT SINK X64ASM:ENC-SBB-MR   s" 49196d00" X=
   \ llvm-mc: sbbq $127, %rsp
   X64ASM:RSP 127 X64ASM:>IMM8 SINK X64ASM:ENC-SBB-RI8   s" 4883dc7f" X=
   \ llvm-mc: sbbq $65536, %rsi
   X64ASM:RSI 65536 X64ASM:>IMM32 SINK X64ASM:ENC-SBB-RI32   s" 4881de00000100" X= ;

\ and and sub. The rsp base is the other SIB-only base, and the
\ scaled-index form carries its own SIB.
: ALU-AND-SUB ( -- )
   \ llvm-mc: andq %rsi, %rdi
   X64ASM:RDI X64ASM:RSI SINK X64ASM:ENC-AND-RR   s" 4821f7" X=
   \ llvm-mc: andq (%rsp), %rdi
   X64ASM:RDI X64ASM:RSP X64ASM:MEM-AT SINK X64ASM:ENC-AND-RM   s" 48233c24" X=
   \ llvm-mc: andq %rdi, (%rsp)
   X64ASM:RDI X64ASM:RSP X64ASM:MEM-AT SINK X64ASM:ENC-AND-MR   s" 48213c24" X=
   \ llvm-mc: andq $-128, %rdi
   X64ASM:RDI -128 X64ASM:>IMM8 SINK X64ASM:ENC-AND-RI8   s" 4883e780" X=
   \ llvm-mc: andq $4096, %rdi
   X64ASM:RDI 4096 X64ASM:>IMM32 SINK X64ASM:ENC-AND-RI32   s" 4881e700100000" X=
   \ llvm-mc: subq %r13, %r14
   X64ASM:R14 X64ASM:R13 SINK X64ASM:ENC-SUB-RR   s" 4d29ee" X=
   \ llvm-mc: subq 8(%rbx,%rcx,4), %rax
   X64ASM:RAX X64ASM:RBX X64ASM:RCX 4 8 X64ASM:MEM-IDX SINK X64ASM:ENC-SUB-RM   s" 482b448b08" X=
   \ llvm-mc: subq %rax, 8(%rbx,%rcx,4)
   X64ASM:RAX X64ASM:RBX X64ASM:RCX 4 8 X64ASM:MEM-IDX SINK X64ASM:ENC-SUB-MR   s" 4829448b08" X=
   \ llvm-mc: subq $1, %rbp
   X64ASM:RBP 1 X64ASM:>IMM8 SINK X64ASM:ENC-SUB-RI8   s" 4883ed01" X=
   \ llvm-mc: subq $100000, %rbp
   X64ASM:RBP 100000 X64ASM:>IMM32 SINK X64ASM:ENC-SUB-RI32   s" 4881eda0860100" X= ;

\ xor and cmp, including the two rip-relative forms and the
\ disp8/disp32 boundary at -128 and -129.
: ALU-XOR-CMP ( -- )
   \ llvm-mc: xorq %rcx, %rcx
   X64ASM:RCX X64ASM:RCX SINK X64ASM:ENC-XOR-RR   s" 4831c9" X=
   \ llvm-mc: xorq (%rip), %r11
   X64ASM:R11 0 X64ASM:MEM-RIP SINK X64ASM:ENC-XOR-RM   s" 4c331d00000000" X=
   \ llvm-mc: xorq %r11, 16(%rip)
   X64ASM:R11 16 X64ASM:MEM-RIP SINK X64ASM:ENC-XOR-MR   s" 4c311d10000000" X=
   \ llvm-mc: xorq $-16, %r15
   X64ASM:R15 -16 X64ASM:>IMM8 SINK X64ASM:ENC-XOR-RI8   s" 4983f7f0" X=
   \ llvm-mc: xorq $65536, %r15
   X64ASM:R15 65536 X64ASM:>IMM32 SINK X64ASM:ENC-XOR-RI32   s" 4981f700000100" X=
   \ llvm-mc: cmpq %r15, %r14
   X64ASM:R14 X64ASM:R15 SINK X64ASM:ENC-CMP-RR   s" 4d39fe" X=
   \ llvm-mc: cmpq -129(%rbx), %rax
   X64ASM:RAX X64ASM:RBX -129 X64ASM:MEM-OFF SINK X64ASM:ENC-CMP-RM   s" 483b837fffffff" X=
   \ llvm-mc: cmpq %rax, -128(%rbx)
   X64ASM:RAX X64ASM:RBX -128 X64ASM:MEM-OFF SINK X64ASM:ENC-CMP-MR   s" 48394380" X=
   \ llvm-mc: cmpq $8, %rcx
   X64ASM:RCX 8 X64ASM:>IMM8 SINK X64ASM:ENC-CMP-RI8   s" 4883f908" X=
   \ llvm-mc: cmpq $1048576, %rcx
   X64ASM:RCX 1048576 X64ASM:>IMM32 SINK X64ASM:ENC-CMP-RI32   s" 4881f900001000" X= ;

\ test has no sign-extended imm8 form, so it has three.
: TEST-FORMS ( -- )
   \ llvm-mc: testq %rbx, %rax
   X64ASM:RAX X64ASM:RBX SINK X64ASM:ENC-TEST-RR   s" 4885d8" X=
   \ llvm-mc: testq %rcx, (%rdi)
   X64ASM:RCX X64ASM:RDI X64ASM:MEM-AT SINK X64ASM:ENC-TEST-MR   s" 48850f" X=
   \ llvm-mc: testq $16, %rcx
   X64ASM:RCX 16 X64ASM:>IMM32 SINK X64ASM:ENC-TEST-RI32   s" 48f7c110000000" X= ;

\ moves, loads and stores at all four widths, and the three
\ constant forms. A 32-bit move zero-extends into the full register; movabs is
\ the relocatable literal whose immediate starts at MOV-RI64-IMM-OFF.
: MOVES ( -- )
   \ llvm-mc: movq %rbx, %rax
   X64ASM:RAX X64ASM:RBX SINK X64ASM:ENC-MOV-RR   s" 4889d8" X=
   \ llvm-mc: movq 8(%r13,%rcx,4), %r9
   X64ASM:R9 X64ASM:R13 X64ASM:RCX 4 8 X64ASM:MEM-IDX SINK X64ASM:ENC-MOV-RM   s" 4d8b4c8d08" X=
   \ llvm-mc: movq %r13, 24(%r12,%r15,8)
   X64ASM:R13 X64ASM:R12 X64ASM:R15 8 24 X64ASM:MEM-IDX SINK X64ASM:ENC-MOV-MR   s" 4f896cfc18" X=
   \ llvm-mc: movl %ebx, %eax
   0 X64ASM:>R32 3 X64ASM:>R32 SINK X64ASM:ENC-MOV32-RR   s" 89d8" X=
   \ llvm-mc: movl (%rdi), %eax
   0 X64ASM:>R32 X64ASM:RDI X64ASM:MEM-AT SINK X64ASM:ENC-MOV32-RM   s" 8b07" X=
   \ llvm-mc: movl %eax, (%rdi)
   0 X64ASM:>R32 X64ASM:RDI X64ASM:MEM-AT SINK X64ASM:ENC-MOV32-MR   s" 8907" X=
   \ llvm-mc: movw %ax, %bx
   3 X64ASM:>R16 0 X64ASM:>R16 SINK X64ASM:ENC-MOV16-RR   s" 6689c3" X=
   \ llvm-mc: movw (%rdi), %ax
   0 X64ASM:>R16 X64ASM:RDI X64ASM:MEM-AT SINK X64ASM:ENC-MOV16-RM   s" 668b07" X=
   \ llvm-mc: movw %ax, (%rdi)
   0 X64ASM:>R16 X64ASM:RDI X64ASM:MEM-AT SINK X64ASM:ENC-MOV16-MR   s" 668907" X=
   \ llvm-mc: movb %al, %sil
   6 X64ASM:>R8 0 X64ASM:>R8 SINK X64ASM:ENC-MOV8-RR   s" 4088c6" X=
   \ llvm-mc: movb (%rdi), %sil
   6 X64ASM:>R8 X64ASM:RDI X64ASM:MEM-AT SINK X64ASM:ENC-MOV8-RM   s" 408a37" X=
   \ llvm-mc: movb %sil, (%rdi)
   6 X64ASM:>R8 X64ASM:RDI X64ASM:MEM-AT SINK X64ASM:ENC-MOV8-MR   s" 408837" X=
   \ llvm-mc: movq $-1, %rax
   X64ASM:RAX -1 X64ASM:>IMM32 SINK X64ASM:ENC-MOV-RI32   s" 48c7c0ffffffff" X=
   \ llvm-mc: movl $4294967295, %eax
   0 X64ASM:>R32 4294967295 X64ASM:>IMM32 SINK X64ASM:ENC-MOV32-RI32   s" b8ffffffff" X=
   \ llvm-mc: movabsq $1234605616436508552, %r11
   X64ASM:R11 $1122334455667788 X64ASM:>IMM64 SINK X64ASM:ENC-MOV-RI64   s" 49bb8877665544332211" X= ;

\ movzx, movsx and movsxd. A byte source at or above 4 is
\ sil/dil and carries REX even when no REX field is set.
: WIDENING ( -- )
   \ llvm-mc: movzbq %sil, %rax
   X64ASM:RAX 6 X64ASM:>R8 SINK X64ASM:ENC-MOVZX-8-RR   s" 480fb6c6" X=
   \ llvm-mc: movzbq (%rdi), %rax
   X64ASM:RAX X64ASM:RDI X64ASM:MEM-AT SINK X64ASM:ENC-MOVZX-8-RM   s" 480fb607" X=
   \ llvm-mc: movzwq %si, %rax
   X64ASM:RAX 6 X64ASM:>R16 SINK X64ASM:ENC-MOVZX-16-RR   s" 480fb7c6" X=
   \ llvm-mc: movzwq (%rdi), %rax
   X64ASM:RAX X64ASM:RDI X64ASM:MEM-AT SINK X64ASM:ENC-MOVZX-16-RM   s" 480fb707" X=
   \ llvm-mc: movsbq %dil, %rax
   X64ASM:RAX 7 X64ASM:>R8 SINK X64ASM:ENC-MOVSX-8-RR   s" 480fbec7" X=
   \ llvm-mc: movsbq (%rdi), %rax
   X64ASM:RAX X64ASM:RDI X64ASM:MEM-AT SINK X64ASM:ENC-MOVSX-8-RM   s" 480fbe07" X=
   \ llvm-mc: movswq %si, %rax
   X64ASM:RAX 6 X64ASM:>R16 SINK X64ASM:ENC-MOVSX-16-RR   s" 480fbfc6" X=
   \ llvm-mc: movswq (%rdi), %rax
   X64ASM:RAX X64ASM:RDI X64ASM:MEM-AT SINK X64ASM:ENC-MOVSX-16-RM   s" 480fbf07" X=
   \ llvm-mc: movslq %ecx, %rax
   X64ASM:RAX 1 X64ASM:>R32 SINK X64ASM:ENC-MOVSXD-RR   s" 4863c1" X=
   \ llvm-mc: movslq (%rdi), %rax
   X64ASM:RAX X64ASM:RDI X64ASM:MEM-AT SINK X64ASM:ENC-MOVSXD-RM   s" 486307" X= ;

\ lea and the 32-bit zeroing idiom.
: ADDRESS-AND-ZERO ( -- )
   \ llvm-mc: leaq 16(%rax,%rbx,8), %rcx
   X64ASM:RCX X64ASM:RAX X64ASM:RBX 8 16 X64ASM:MEM-IDX SINK X64ASM:ENC-LEA   s" 488d4cd810" X=
   \ llvm-mc: xorl %eax, %eax
   0 X64ASM:>R32 0 X64ASM:>R32 SINK X64ASM:ENC-XOR32-RR   s" 31c0" X= ;

\ the F7 and FF groups and cqo.
: ONE-REGISTER ( -- )
   \ llvm-mc: notq %r15
   X64ASM:R15 SINK X64ASM:ENC-NOT   s" 49f7d7" X=
   \ llvm-mc: negq %rax
   X64ASM:RAX SINK X64ASM:ENC-NEG   s" 48f7d8" X=
   \ llvm-mc: mulq %rcx
   X64ASM:RCX SINK X64ASM:ENC-MUL   s" 48f7e1" X=
   \ llvm-mc: imulq %rcx
   X64ASM:RCX SINK X64ASM:ENC-IMUL1   s" 48f7e9" X=
   \ llvm-mc: divq %rcx
   X64ASM:RCX SINK X64ASM:ENC-DIV   s" 48f7f1" X=
   \ llvm-mc: idivq %rcx
   X64ASM:RCX SINK X64ASM:ENC-IDIV   s" 48f7f9" X=
   \ llvm-mc: incq %rax
   X64ASM:RAX SINK X64ASM:ENC-INC   s" 48ffc0" X=
   \ llvm-mc: decq %r8
   X64ASM:R8 SINK X64ASM:ENC-DEC   s" 49ffc8" X=
   \ llvm-mc: cqto
   SINK X64ASM:ENC-CQO   s" 4899" X= ;

\ the non-widening imul in its four forms.
: SIGNED-MULTIPLY ( -- )
   \ llvm-mc: imulq %rcx, %rax
   X64ASM:RAX X64ASM:RCX SINK X64ASM:ENC-IMUL-RR   s" 480fafc1" X=
   \ llvm-mc: imulq (%rdi), %rax
   X64ASM:RAX X64ASM:RDI X64ASM:MEM-AT SINK X64ASM:ENC-IMUL-RM   s" 480faf07" X=
   \ llvm-mc: imulq $3, %rcx, %rax
   X64ASM:RAX X64ASM:RCX 3 X64ASM:>IMM8 SINK X64ASM:ENC-IMUL-RRI8   s" 486bc103" X=
   \ llvm-mc: imulq $300, %rcx, %rax
   X64ASM:RAX X64ASM:RCX 300 X64ASM:>IMM32 SINK X64ASM:ENC-IMUL-RRI32   s" 4869c12c010000" X= ;

\ shifts and rotates by imm8 and by cl.
: SHIFTS ( -- )
   \ llvm-mc: rolq $4, %rax
   X64ASM:RAX 4 X64ASM:>IMM8 SINK X64ASM:ENC-ROL-RI8   s" 48c1c004" X=
   \ llvm-mc: rorq $2, %rbx
   X64ASM:RBX 2 X64ASM:>IMM8 SINK X64ASM:ENC-ROR-RI8   s" 48c1cb02" X=
   \ llvm-mc: shlq $3, %rax
   X64ASM:RAX 3 X64ASM:>IMM8 SINK X64ASM:ENC-SHL-RI8   s" 48c1e003" X=
   \ llvm-mc: shrq $63, %rax
   X64ASM:RAX 63 X64ASM:>IMM8 SINK X64ASM:ENC-SHR-RI8   s" 48c1e83f" X=
   \ llvm-mc: sarq $2, %r9
   X64ASM:R9 2 X64ASM:>IMM8 SINK X64ASM:ENC-SAR-RI8   s" 49c1f902" X=
   \ llvm-mc: rolq %cl, %rax
   X64ASM:RAX SINK X64ASM:ENC-ROL-CL   s" 48d3c0" X=
   \ llvm-mc: rorq %cl, %rbx
   X64ASM:RBX SINK X64ASM:ENC-ROR-CL   s" 48d3cb" X=
   \ llvm-mc: shlq %cl, %r10
   X64ASM:R10 SINK X64ASM:ENC-SHL-CL   s" 49d3e2" X=
   \ llvm-mc: shrq %cl, %rax
   X64ASM:RAX SINK X64ASM:ENC-SHR-CL   s" 48d3e8" X=
   \ llvm-mc: sarq %cl, %rax
   X64ASM:RAX SINK X64ASM:ENC-SAR-CL   s" 48d3f8" X= ;

\ indirect branches, return, syscall, setcc, cmovcc, the stack
\ and the exchange.
: CONTROL ( -- )
   \ llvm-mc: jmpq *%rax
   X64ASM:RAX SINK X64ASM:ENC-JMP-REG   s" ffe0" X=
   \ llvm-mc: jmpq *%r11
   X64ASM:R11 SINK X64ASM:ENC-JMP-REG   s" 41ffe3" X=
   \ llvm-mc: callq *%rax
   X64ASM:RAX SINK X64ASM:ENC-CALL-REG   s" ffd0" X=
   \ llvm-mc: ret
   SINK X64ASM:ENC-RET   s" c3" X=
   \ llvm-mc: syscall
   SINK X64ASM:ENC-SYSCALL   s" 0f05" X=
   \ llvm-mc: setl %al
   X64ASM:C-L 0 X64ASM:>R8 SINK X64ASM:ENC-SETCC   s" 0f9cc0" X=
   \ llvm-mc: setl %sil
   X64ASM:C-L 6 X64ASM:>R8 SINK X64ASM:ENC-SETCC   s" 400f9cc6" X=
   \ llvm-mc: cmovlq %rcx, %rax
   X64ASM:C-L X64ASM:RAX X64ASM:RCX SINK X64ASM:ENC-CMOVCC   s" 480f4cc1" X=
   \ llvm-mc: pushq %rax
   X64ASM:RAX SINK X64ASM:ENC-PUSH   s" 50" X=
   \ llvm-mc: pushq %r13
   X64ASM:R13 SINK X64ASM:ENC-PUSH   s" 4155" X=
   \ llvm-mc: popq %rbp
   X64ASM:RBP SINK X64ASM:ENC-POP   s" 5d" X=
   \ llvm-mc: popq %r12
   X64ASM:R12 SINK X64ASM:ENC-POP   s" 415c" X=
   \ llvm-mc: xchgq %rbx, %rcx
   X64ASM:RCX X64ASM:RBX SINK X64ASM:ENC-XCHG-RR   s" 4887cb" X= ;

\ the atomic forms. The memory xchg is locked with no prefix; xadd and cmpxchg
\ carry f0 ahead of REX, here with REX.R and REX.B or REX.X set behind it.
: ATOMICS ( -- )
   \ llvm-mc: xchgq %rax, (%rcx)
   X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT SINK X64ASM:ENC-XCHG-MR   s" 488701" X=
   \ llvm-mc: xchgq %r9, 8(%r12)
   X64ASM:R9 X64ASM:R12 8 X64ASM:MEM-OFF SINK X64ASM:ENC-XCHG-MR   s" 4d874c2408" X=
   \ llvm-mc: xchgl %eax, (%rcx)
   0 X64ASM:>R32 X64ASM:RCX X64ASM:MEM-AT SINK X64ASM:ENC-XCHG32-MR   s" 8701" X=
   \ llvm-mc: xchgl %r9d, 8(%r12)
   9 X64ASM:>R32 X64ASM:R12 8 X64ASM:MEM-OFF SINK X64ASM:ENC-XCHG32-MR   s" 45874c2408" X=
   \ llvm-mc: lock xaddq %rax, (%rcx)
   X64ASM:RAX X64ASM:RCX X64ASM:MEM-AT SINK X64ASM:ENC-LOCK-XADD-MR   s" f0480fc101" X=
   \ llvm-mc: lock xaddq %r10, (%r13)
   X64ASM:R10 X64ASM:R13 X64ASM:MEM-AT SINK X64ASM:ENC-LOCK-XADD-MR   s" f04d0fc15500" X=
   \ llvm-mc: lock cmpxchgq %rdx, (%rcx)
   X64ASM:RDX X64ASM:RCX X64ASM:MEM-AT SINK X64ASM:ENC-LOCK-CMPXCHG-MR   s" f0480fb111" X=
   \ llvm-mc: lock cmpxchgq %rbx, -16(%rbp,%r8,8)
   X64ASM:RBX X64ASM:RBP X64ASM:R8 8 -16 X64ASM:MEM-IDX SINK X64ASM:ENC-LOCK-CMPXCHG-MR   s" f04a0fb15cc5f0" X=
   \ llvm-mc: mfence
   SINK X64ASM:ENC-MFENCE   s" 0faef0" X= ;

\ the addressing forms that have no ARM counterpart: rsp and
\ r12 forcing a SIB, rbp and r13 forcing a disp8 of zero, the disp8 boundary,
\ rip-relative, and an rsp base carrying an index.
: MEMORY-TRAPS ( -- )
   \ llvm-mc: movq (%rsp), %rax
   X64ASM:RAX X64ASM:RSP X64ASM:MEM-AT SINK X64ASM:ENC-MOV-RM   s" 488b0424" X=
   \ llvm-mc: movq (%rbp), %rax
   X64ASM:RAX X64ASM:RBP X64ASM:MEM-AT SINK X64ASM:ENC-MOV-RM   s" 488b4500" X=
   \ llvm-mc: movq (%r12), %rax
   X64ASM:RAX X64ASM:R12 X64ASM:MEM-AT SINK X64ASM:ENC-MOV-RM   s" 498b0424" X=
   \ llvm-mc: movq (%r13), %rax
   X64ASM:RAX X64ASM:R13 X64ASM:MEM-AT SINK X64ASM:ENC-MOV-RM   s" 498b4500" X=
   \ llvm-mc: movq -128(%rbx), %rax
   X64ASM:RAX X64ASM:RBX -128 X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-RM   s" 488b4380" X=
   \ llvm-mc: movq -129(%rbx), %rax
   X64ASM:RAX X64ASM:RBX -129 X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-RM   s" 488b837fffffff" X=
   \ llvm-mc: movq (%rip), %rax
   X64ASM:RAX 0 X64ASM:MEM-RIP SINK X64ASM:ENC-MOV-RM   s" 488b0500000000" X=
   \ llvm-mc: movq 16(%rip), %rax
   X64ASM:RAX 16 X64ASM:MEM-RIP SINK X64ASM:ENC-MOV-RM   s" 488b0510000000" X=
   \ llvm-mc: movq %r15, 4(%rsp,%rbp,2)
   X64ASM:R15 X64ASM:RSP X64ASM:RBP 2 4 X64ASM:MEM-IDX SINK X64ASM:ENC-MOV-MR   s" 4c897c6c04" X= ;
\ The sixteen condition names, each through the one-byte conditional jump, so a
\ transposed code cannot hide: the opcode IS 70 plus the condition.
: CONDITION-NAMES ( -- )
   \ llvm-objdump: jo (to the next instruction)
   X64ASM:C-O 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7000" X=
   \ llvm-objdump: jno (to the next instruction)
   X64ASM:C-NO 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7100" X=
   \ llvm-objdump: jb (to the next instruction)
   X64ASM:C-B 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7200" X=
   \ llvm-objdump: jae (to the next instruction)
   X64ASM:C-AE 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7300" X=
   \ llvm-objdump: je (to the next instruction)
   X64ASM:C-E 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7400" X=
   \ llvm-objdump: jne (to the next instruction)
   X64ASM:C-NE 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7500" X=
   \ llvm-objdump: jbe (to the next instruction)
   X64ASM:C-BE 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7600" X=
   \ llvm-objdump: ja (to the next instruction)
   X64ASM:C-A 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7700" X=
   \ llvm-objdump: js (to the next instruction)
   X64ASM:C-S 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7800" X=
   \ llvm-objdump: jns (to the next instruction)
   X64ASM:C-NS 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7900" X=
   \ llvm-objdump: jp (to the next instruction)
   X64ASM:C-P 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7a00" X=
   \ llvm-objdump: jnp (to the next instruction)
   X64ASM:C-NP 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7b00" X=
   \ llvm-objdump: jl (to the next instruction)
   X64ASM:C-L 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7c00" X=
   \ llvm-objdump: jge (to the next instruction)
   X64ASM:C-GE 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7d00" X=
   \ llvm-objdump: jle (to the next instruction)
   X64ASM:C-LE 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7e00" X=
   \ llvm-objdump: jg (to the next instruction)
   X64ASM:C-G 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7f00" X= ;

\ The sixteen register names, each through push, so a misnumbered name cannot
\ hide: the opcode IS 50 plus the low three bits, with REX.B above seven.
: REGISTER-NAMES ( -- )
   \ llvm-objdump: pushq %rax
   X64ASM:RAX SINK X64ASM:ENC-PUSH   s" 50" X=
   \ llvm-objdump: pushq %rcx
   X64ASM:RCX SINK X64ASM:ENC-PUSH   s" 51" X=
   \ llvm-objdump: pushq %rdx
   X64ASM:RDX SINK X64ASM:ENC-PUSH   s" 52" X=
   \ llvm-objdump: pushq %rbx
   X64ASM:RBX SINK X64ASM:ENC-PUSH   s" 53" X=
   \ llvm-objdump: pushq %rsp
   X64ASM:RSP SINK X64ASM:ENC-PUSH   s" 54" X=
   \ llvm-objdump: pushq %rbp
   X64ASM:RBP SINK X64ASM:ENC-PUSH   s" 55" X=
   \ llvm-objdump: pushq %rsi
   X64ASM:RSI SINK X64ASM:ENC-PUSH   s" 56" X=
   \ llvm-objdump: pushq %rdi
   X64ASM:RDI SINK X64ASM:ENC-PUSH   s" 57" X=
   \ llvm-objdump: pushq %r8
   X64ASM:R8 SINK X64ASM:ENC-PUSH   s" 4150" X=
   \ llvm-objdump: pushq %r9
   X64ASM:R9 SINK X64ASM:ENC-PUSH   s" 4151" X=
   \ llvm-objdump: pushq %r10
   X64ASM:R10 SINK X64ASM:ENC-PUSH   s" 4152" X=
   \ llvm-objdump: pushq %r11
   X64ASM:R11 SINK X64ASM:ENC-PUSH   s" 4153" X=
   \ llvm-objdump: pushq %r12
   X64ASM:R12 SINK X64ASM:ENC-PUSH   s" 4154" X=
   \ llvm-objdump: pushq %r13
   X64ASM:R13 SINK X64ASM:ENC-PUSH   s" 4155" X=
   \ llvm-objdump: pushq %r14
   X64ASM:R14 SINK X64ASM:ENC-PUSH   s" 4156" X=
   \ llvm-objdump: pushq %r15
   X64ASM:R15 SINK X64ASM:ENC-PUSH   s" 4157" X= ;

: PADDING-AND-TRAP ( -- )
   \ llvm-mc: nop
   SINK X64ASM:ENC-NOP   s" 90" X=
   \ llvm-mc: ud2
   SINK X64ASM:ENC-UD2   s" 0f0b" X= ;

: IMMEDIATE-STORES ( -- )
   \ llvm-mc: movl $0x12345678, 16(%rip)
   $12345678 X64ASM:>IMM32 16 X64ASM:MEM-RIP SINK X64ASM:ENC-MOV32-MI32   s" c7051000000078563412" X=
   \ llvm-mc: movl $-1, -129(%r13,%r12,8)
   -1 X64ASM:>IMM32 X64ASM:R13 X64ASM:R12 8 -129 X64ASM:MEM-IDX SINK X64ASM:ENC-MOV32-MI32   s" 43c784e57fffffffffffffff" X=
   \ llvm-mc: movl $0, (%rax)
   0 X64ASM:>IMM32 X64ASM:RAX X64ASM:MEM-AT SINK X64ASM:ENC-MOV32-MI32   s" c70000000000" X=
   \ llvm-mc: movl $-2147483648, (%r13)
   -2147483648 X64ASM:>IMM32 X64ASM:R13 X64ASM:MEM-AT SINK X64ASM:ENC-MOV32-MI32   s" 41c7450000000080" X=
   \ llvm-mc: movl $2147483647, 127(%rsp)
   2147483647 X64ASM:>IMM32 X64ASM:RSP 127 X64ASM:MEM-OFF SINK X64ASM:ENC-MOV32-MI32   s" c744247fffffff7f" X=
   \ llvm-mc: movl $1, 128(%r12)
   1 X64ASM:>IMM32 X64ASM:R12 128 X64ASM:MEM-OFF SINK X64ASM:ENC-MOV32-MI32   s" 41c784248000000001000000" X=
   \ llvm-mc: movl $-2, -128(%rbp)
   -2 X64ASM:>IMM32 X64ASM:RBP -128 X64ASM:MEM-OFF SINK X64ASM:ENC-MOV32-MI32   s" c74580feffffff" X=
   \ llvm-objdump: .Lhere: movl $7, .Lhere(%rip)
   \ Target is the instruction's beginning: -10 includes the trailing imm32.
   7 X64ASM:>IMM32 -10 X64ASM:MEM-RIP SINK X64ASM:ENC-MOV32-MI32
   SINK-LEN 10 T=   s" c705f6ffffff07000000" X= ;

: CELL-STORES ( -- )
   \ llvm-mc: movq $0, (%r12)
   0 X64ASM:>IMM32 X64ASM:R12 X64ASM:MEM-AT SINK X64ASM:ENC-MOV-MI32   s" 49c7042400000000" X=
   \ llvm-mc: movq $-1, -8(%r12)
   -1 X64ASM:>IMM32 X64ASM:R12 -8 X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-MI32   s" 49c74424f8ffffffff" X=
   \ llvm-mc: movq $-2147483648, 128(%rbx)
   -2147483648 X64ASM:>IMM32 X64ASM:RBX 128 X64ASM:MEM-OFF SINK X64ASM:ENC-MOV-MI32   s" 48c7838000000000000080" X=
   \ llvm-mc: movq $2147483647, (%r13)
   2147483647 X64ASM:>IMM32 X64ASM:R13 X64ASM:MEM-AT SINK X64ASM:ENC-MOV-MI32   s" 49c74500ffffff7f" X=
   [: 2147483648 X64ASM:>IMM32 X64ASM:RAX X64ASM:MEM-AT SINK X64ASM:ENC-MOV-MI32 ;] X64ASM:E-OPERAND TTHROWSQ ;

: STORE-REJECTIONS ( -- )
   X64ASM:RAX SINK X64ASM:ENC-PUSH
   [: 2147483648 X64ASM:>IMM32 X64ASM:RAX X64ASM:MEM-AT SINK X64ASM:ENC-MOV32-MI32 ;] X64ASM:E-OPERAND TTHROWSQ
   [: -2147483649 X64ASM:>IMM32 X64ASM:RAX X64ASM:MEM-AT SINK X64ASM:ENC-MOV32-MI32 ;] X64ASM:E-OPERAND TTHROWSQ
   s" 50" X=
   s" X64-GOOD-STORE ( X64ASM:imm32 X64ASM:mem ^VEC:vector<u8> -- ) X64ASM:ENC-MOV32-MI32"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" X64-BAD-STORE ( X64ASM:imm64 X64ASM:mem ^VEC:vector<u8> -- ) X64ASM:ENC-MOV32-MI32"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: SSE-REGISTERS ( -- )
   \ llvm-mc: movsd %xmm0, %xmm0
   X64ASM:XMM0 X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR   s" f20f10c0" X=
   \ llvm-mc: movsd %xmm0, %xmm1
   X64ASM:XMM1 X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR   s" f20f10c8" X=
   \ llvm-mc: movsd %xmm0, %xmm2
   X64ASM:XMM2 X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR   s" f20f10d0" X=
   \ llvm-mc: movsd %xmm0, %xmm3
   X64ASM:XMM3 X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR   s" f20f10d8" X=
   \ llvm-mc: movsd %xmm0, %xmm4
   X64ASM:XMM4 X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR   s" f20f10e0" X=
   \ llvm-mc: movsd %xmm0, %xmm5
   X64ASM:XMM5 X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR   s" f20f10e8" X=
   \ llvm-mc: movsd %xmm0, %xmm6
   X64ASM:XMM6 X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR   s" f20f10f0" X=
   \ llvm-mc: movsd %xmm0, %xmm7
   X64ASM:XMM7 X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR   s" f20f10f8" X=
   \ llvm-mc: movsd %xmm0, %xmm8
   X64ASM:XMM8 X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR   s" f2440f10c0" X=
   \ llvm-mc: movsd %xmm0, %xmm9
   X64ASM:XMM9 X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR   s" f2440f10c8" X=
   \ llvm-mc: movsd %xmm0, %xmm10
   X64ASM:XMM10 X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR   s" f2440f10d0" X=
   \ llvm-mc: movsd %xmm0, %xmm11
   X64ASM:XMM11 X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR   s" f2440f10d8" X=
   \ llvm-mc: movsd %xmm0, %xmm12
   X64ASM:XMM12 X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR   s" f2440f10e0" X=
   \ llvm-mc: movsd %xmm0, %xmm13
   X64ASM:XMM13 X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR   s" f2440f10e8" X=
   \ llvm-mc: movsd %xmm0, %xmm14
   X64ASM:XMM14 X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR   s" f2440f10f0" X=
   \ llvm-mc: movsd %xmm0, %xmm15
   X64ASM:XMM15 X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR   s" f2440f10f8" X= ;

: SSE-MOVES ( -- )
   \ llvm-mc: movsd %xmm15, %xmm8
   X64ASM:XMM8 X64ASM:XMM15 SINK X64ASM:ENC-MOVSD-RR   s" f2450f10c7" X=
   \ llvm-mc: movsd %xmm15, %xmm0
   X64ASM:XMM0 X64ASM:XMM15 SINK X64ASM:ENC-MOVSD-RR   s" f2410f10c7" X=
   \ llvm-mc: movsd 8(%r13,%r12,4), %xmm9
   X64ASM:XMM9 X64ASM:R13 X64ASM:R12 4 8 X64ASM:MEM-IDX SINK X64ASM:ENC-MOVSD-RM   s" f2470f104ca508" X=
   \ llvm-mc: movsd %xmm15, -129(%r12)
   X64ASM:XMM15 X64ASM:R12 -129 X64ASM:MEM-OFF SINK X64ASM:ENC-MOVSD-MR   s" f2450f11bc247fffffff" X=
   \ llvm-mc: movsd 16(%rip), %xmm8
   X64ASM:XMM8 16 X64ASM:MEM-RIP SINK X64ASM:ENC-MOVSD-RM   s" f2440f100510000000" X=
   \ llvm-mc: movsd %xmm1, -16(%rip)
   X64ASM:XMM1 -16 X64ASM:MEM-RIP SINK X64ASM:ENC-MOVSD-MR   s" f20f110df0ffffff" X=
   \ llvm-mc: movsd (%rbp), %xmm0
   X64ASM:XMM0 X64ASM:RBP X64ASM:MEM-AT SINK X64ASM:ENC-MOVSD-RM   s" f20f104500" X=
   \ llvm-mc: movsd (%rsp), %xmm1
   X64ASM:XMM1 X64ASM:RSP X64ASM:MEM-AT SINK X64ASM:ENC-MOVSD-RM   s" f20f100c24" X= ;

: SSE-ARITHMETIC ( -- )
   \ llvm-mc: addsd %xmm9, %xmm8
   X64ASM:XMM8 X64ASM:XMM9 SINK X64ASM:ENC-ADDSD-RR   s" f2450f58c1" X=
   \ llvm-mc: subsd %xmm9, %xmm8
   X64ASM:XMM8 X64ASM:XMM9 SINK X64ASM:ENC-SUBSD-RR   s" f2450f5cc1" X=
   \ llvm-mc: mulsd %xmm9, %xmm8
   X64ASM:XMM8 X64ASM:XMM9 SINK X64ASM:ENC-MULSD-RR   s" f2450f59c1" X=
   \ llvm-mc: divsd %xmm9, %xmm8
   X64ASM:XMM8 X64ASM:XMM9 SINK X64ASM:ENC-DIVSD-RR   s" f2450f5ec1" X=
   \ llvm-mc: sqrtsd %xmm9, %xmm8
   X64ASM:XMM8 X64ASM:XMM9 SINK X64ASM:ENC-SQRTSD-RR   s" f2450f51c1" X=
   \ llvm-mc: andpd %xmm15, %xmm8
   X64ASM:XMM8 X64ASM:XMM15 SINK X64ASM:ENC-ANDPD-RR   s" 66450f54c7" X=
   \ llvm-mc: xorpd %xmm15, %xmm8
   X64ASM:XMM8 X64ASM:XMM15 SINK X64ASM:ENC-XORPD-RR   s" 66450f57c7" X=
   \ llvm-mc: cmpunordsd %xmm15, %xmm8
   X64ASM:XMM8 X64ASM:XMM15 SINK X64ASM:ENC-CMPUNORDSD-RR   s" f2450fc2c703" X=
   \ llvm-mc: ucomisd %xmm15, %xmm8
   X64ASM:XMM8 X64ASM:XMM15 SINK X64ASM:ENC-UCOMISD-RR   s" 66450f2ec7" X=
   \ llvm-mc: cvtsi2sdq %r15, %xmm8
   X64ASM:XMM8 X64ASM:R15 SINK X64ASM:ENC-CVTSI2SD-RR   s" f24d0f2ac7" X=
   \ llvm-mc: cvttsd2siq %xmm15, %r8
   X64ASM:R8 X64ASM:XMM15 SINK X64ASM:ENC-CVTTSD2SI-RR   s" f24d0f2cc7" X=
   \ llvm-mc: cvtsi2sdq %rax, %xmm0
   X64ASM:XMM0 X64ASM:RAX SINK X64ASM:ENC-CVTSI2SD-RR   s" f2480f2ac0" X=
   \ llvm-mc: cvttsd2siq %xmm1, %rcx
   X64ASM:RCX X64ASM:XMM1 SINK X64ASM:ENC-CVTTSD2SI-RR   s" f2480f2cc9" X= ;

\ The packed moves and shuffles, and the three-byte opcodes 0F38xx and 0F3Axx.
: SSE-PACKED ( -- )
   \ llvm-mc: movdqu 16(%rsi), %xmm3
   X64ASM:XMM3 X64ASM:RSI 16 X64ASM:MEM-OFF SINK X64ASM:ENC-MOVDQU-RM   s" f30f6f5e10" X=
   \ llvm-mc: movdqu -129(%r12), %xmm15
   X64ASM:XMM15 X64ASM:R12 -129 X64ASM:MEM-OFF SINK X64ASM:ENC-MOVDQU-RM   s" f3450f6fbc247fffffff" X=
   \ llvm-mc: movdqu %xmm0, 32(%rdi)
   X64ASM:XMM0 X64ASM:RDI 32 X64ASM:MEM-OFF SINK X64ASM:ENC-MOVDQU-MR   s" f30f7f4720" X=
   \ llvm-mc: movdqu %xmm9, 8(%r13,%r12,4)
   X64ASM:XMM9 X64ASM:R13 X64ASM:R12 4 8 X64ASM:MEM-IDX SINK X64ASM:ENC-MOVDQU-MR   s" f3470f7f4ca508" X=
   \ llvm-mc: movdqa %xmm1, %xmm9
   X64ASM:XMM9 X64ASM:XMM1 SINK X64ASM:ENC-MOVDQA-RR   s" 66440f6fc9" X=
   \ llvm-mc: paddd %xmm9, %xmm1
   X64ASM:XMM1 X64ASM:XMM9 SINK X64ASM:ENC-PADDD-RR   s" 66410ffec9" X=
   \ llvm-mc: punpcklqdq %xmm3, %xmm1
   X64ASM:XMM1 X64ASM:XMM3 SINK X64ASM:ENC-PUNPCKLQDQ-RR   s" 660f6ccb" X=
   \ llvm-mc: pshufd $14, %xmm0, %xmm0
   X64ASM:XMM0 X64ASM:XMM0 14 X64ASM:>IMM8 SINK X64ASM:ENC-PSHUFD-RRI8   s" 660f70c00e" X=
   \ llvm-mc: pshufd $255, %xmm15, %xmm8
   X64ASM:XMM8 X64ASM:XMM15 255 X64ASM:>IMM8 SINK X64ASM:ENC-PSHUFD-RRI8   s" 66450f70c7ff" X=
   \ llvm-mc: pshufb %xmm8, %xmm3
   X64ASM:XMM3 X64ASM:XMM8 SINK X64ASM:ENC-PSHUFB-RR   s" 66410f3800d8" X=
   \ llvm-mc: palignr $4, %xmm5, %xmm7
   X64ASM:XMM7 X64ASM:XMM5 4 X64ASM:>IMM8 SINK X64ASM:ENC-PALIGNR-RRI8   s" 660f3a0ffd04" X=
   \ llvm-mc: palignr $4, %xmm15, %xmm8
   X64ASM:XMM8 X64ASM:XMM15 4 X64ASM:>IMM8 SINK X64ASM:ENC-PALIGNR-RRI8   s" 66450f3a0fc704" X=
   \ llvm-mc: pmovzxdq %xmm8, %xmm15
   X64ASM:XMM15 X64ASM:XMM8 SINK X64ASM:ENC-PMOVZXDQ-RR   s" 66450f3835f8" X= ;

: SHA-ROUNDS ( -- )
   \ llvm-mc: sha256rnds2 %xmm0, %xmm1, %xmm2
   X64ASM:XMM2 X64ASM:XMM1 SINK X64ASM:ENC-SHA256RNDS2-RR   s" 0f38cbd1" X=
   \ llvm-mc: sha256rnds2 %xmm0, %xmm8, %xmm15
   X64ASM:XMM15 X64ASM:XMM8 SINK X64ASM:ENC-SHA256RNDS2-RR   s" 450f38cbf8" X=
   \ llvm-mc: sha256msg1 %xmm4, %xmm3
   X64ASM:XMM3 X64ASM:XMM4 SINK X64ASM:ENC-SHA256MSG1-RR   s" 0f38ccdc" X=
   \ llvm-mc: sha256msg1 %xmm9, %xmm8
   X64ASM:XMM8 X64ASM:XMM9 SINK X64ASM:ENC-SHA256MSG1-RR   s" 450f38ccc1" X=
   \ llvm-mc: sha256msg2 %xmm6, %xmm3
   X64ASM:XMM3 X64ASM:XMM6 SINK X64ASM:ENC-SHA256MSG2-RR   s" 0f38cdde" X=
   \ llvm-mc: sha256msg2 %xmm15, %xmm8
   X64ASM:XMM8 X64ASM:XMM15 SINK X64ASM:ENC-SHA256MSG2-RR   s" 450f38cdc7" X= ;

\ movq moves eight bytes between the two files unchanged. The XMM register is the
\ ModRM reg field in both directions, so each pair crosses REX.R with REX.B.
: SSE-CROSS ( -- )
   \ llvm-mc: movq %rax, %xmm0
   X64ASM:XMM0 X64ASM:RAX SINK X64ASM:ENC-MOVQ-XR   s" 66480f6ec0" X=
   \ llvm-mc: movq %xmm0, %rax
   X64ASM:RAX X64ASM:XMM0 SINK X64ASM:ENC-MOVQ-RX   s" 66480f7ec0" X=
   \ llvm-mc: movq %r8, %xmm0
   X64ASM:XMM0 X64ASM:R8 SINK X64ASM:ENC-MOVQ-XR   s" 66490f6ec0" X=
   \ llvm-mc: movq %xmm0, %r8
   X64ASM:R8 X64ASM:XMM0 SINK X64ASM:ENC-MOVQ-RX   s" 66490f7ec0" X=
   \ llvm-mc: movq %r15, %xmm0
   X64ASM:XMM0 X64ASM:R15 SINK X64ASM:ENC-MOVQ-XR   s" 66490f6ec7" X=
   \ llvm-mc: movq %xmm0, %r15
   X64ASM:R15 X64ASM:XMM0 SINK X64ASM:ENC-MOVQ-RX   s" 66490f7ec7" X=
   \ llvm-mc: movq %rax, %xmm8
   X64ASM:XMM8 X64ASM:RAX SINK X64ASM:ENC-MOVQ-XR   s" 664c0f6ec0" X=
   \ llvm-mc: movq %xmm8, %rax
   X64ASM:RAX X64ASM:XMM8 SINK X64ASM:ENC-MOVQ-RX   s" 664c0f7ec0" X=
   \ llvm-mc: movq %r8, %xmm8
   X64ASM:XMM8 X64ASM:R8 SINK X64ASM:ENC-MOVQ-XR   s" 664d0f6ec0" X=
   \ llvm-mc: movq %xmm8, %r8
   X64ASM:R8 X64ASM:XMM8 SINK X64ASM:ENC-MOVQ-RX   s" 664d0f7ec0" X=
   \ llvm-mc: movq %r15, %xmm8
   X64ASM:XMM8 X64ASM:R15 SINK X64ASM:ENC-MOVQ-XR   s" 664d0f6ec7" X=
   \ llvm-mc: movq %xmm8, %r15
   X64ASM:R15 X64ASM:XMM8 SINK X64ASM:ENC-MOVQ-RX   s" 664d0f7ec7" X=
   \ llvm-mc: movq %rax, %xmm15
   X64ASM:XMM15 X64ASM:RAX SINK X64ASM:ENC-MOVQ-XR   s" 664c0f6ef8" X=
   \ llvm-mc: movq %xmm15, %rax
   X64ASM:RAX X64ASM:XMM15 SINK X64ASM:ENC-MOVQ-RX   s" 664c0f7ef8" X=
   \ llvm-mc: movq %r8, %xmm15
   X64ASM:XMM15 X64ASM:R8 SINK X64ASM:ENC-MOVQ-XR   s" 664d0f6ef8" X=
   \ llvm-mc: movq %xmm15, %r8
   X64ASM:R8 X64ASM:XMM15 SINK X64ASM:ENC-MOVQ-RX   s" 664d0f7ef8" X=
   \ llvm-mc: movq %r15, %xmm15
   X64ASM:XMM15 X64ASM:R15 SINK X64ASM:ENC-MOVQ-XR   s" 664d0f6eff" X=
   \ llvm-mc: movq %xmm15, %r15
   X64ASM:R15 X64ASM:XMM15 SINK X64ASM:ENC-MOVQ-RX   s" 664d0f7eff" X= ;

: SSE-REJECTIONS ( -- )
   \ Preserve an existing instruction as well as emitting no partial prefix.
   X64ASM:RAX SINK X64ASM:ENC-PUSH
   [: 16 X64ASM:>XMM X64ASM:XMM0 SINK X64ASM:ENC-MOVSD-RR ;] X64ASM:E-OPERAND TTHROWSQ
   [: X64ASM:XMM0 -1 X64ASM:>XMM SINK X64ASM:ENC-ADDSD-RR ;] X64ASM:E-OPERAND TTHROWSQ
   [: X64ASM:XMM0 16 X64ASM:>R64 SINK X64ASM:ENC-CVTSI2SD-RR ;] X64ASM:E-OPERAND TTHROWSQ
   [: -1 X64ASM:>R64 X64ASM:XMM0 SINK X64ASM:ENC-CVTTSD2SI-RR ;] X64ASM:E-OPERAND TTHROWSQ
   [: 16 X64ASM:>XMM X64ASM:RAX X64ASM:MEM-AT SINK X64ASM:ENC-MOVSD-RM ;] X64ASM:E-OPERAND TTHROWSQ
   [: -1 X64ASM:>XMM X64ASM:RAX X64ASM:MEM-AT SINK X64ASM:ENC-MOVSD-MR ;] X64ASM:E-OPERAND TTHROWSQ
   [: 16 X64ASM:>XMM X64ASM:RAX SINK X64ASM:ENC-MOVQ-XR ;] X64ASM:E-OPERAND TTHROWSQ
   [: X64ASM:XMM0 16 X64ASM:>R64 SINK X64ASM:ENC-MOVQ-XR ;] X64ASM:E-OPERAND TTHROWSQ
   [: -1 X64ASM:>R64 X64ASM:XMM0 SINK X64ASM:ENC-MOVQ-RX ;] X64ASM:E-OPERAND TTHROWSQ
   [: X64ASM:RAX 16 X64ASM:>XMM SINK X64ASM:ENC-MOVQ-RX ;] X64ASM:E-OPERAND TTHROWSQ
   [: 16 X64ASM:>XMM X64ASM:RAX X64ASM:MEM-AT SINK X64ASM:ENC-MOVDQU-RM ;] X64ASM:E-OPERAND TTHROWSQ
   [: X64ASM:XMM0 X64ASM:XMM0 256 X64ASM:>IMM8 SINK X64ASM:ENC-PSHUFD-RRI8 ;] X64ASM:E-OPERAND TTHROWSQ
   [: X64ASM:XMM0 X64ASM:XMM0 -1 X64ASM:>IMM8 SINK X64ASM:ENC-PALIGNR-RRI8 ;] X64ASM:E-OPERAND TTHROWSQ
   [: X64ASM:XMM0 16 X64ASM:>XMM SINK X64ASM:ENC-SHA256RNDS2-RR ;] X64ASM:E-OPERAND TTHROWSQ
   s" 50" X= ;

\ The five relative branches. A displacement is measured from the END of the
\ instruction, so the -12 cases jump back over ten bytes plus their own two, and
\ the rel32 cases back over two hundred plus their own five or six. Bytes read
\ with llvm-objdump from an object llvm-mc assembled from `1: .space N` / `jl 1b`.
: BRANCHES ( -- )
   \ llvm-objdump: eb f4  jmp  (10 bytes back)
   -12 X64ASM:>REL SINK X64ASM:ENC-JMP-REL8   s" ebf4" X=
   \ llvm-objdump: e9 33 ff ff ff  jmp  (200 bytes back)
   -205 X64ASM:>REL SINK X64ASM:ENC-JMP-REL32   s" e933ffffff" X=
   \ llvm-objdump: e8 33 ff ff ff  callq  (200 bytes back)
   -205 X64ASM:>REL SINK X64ASM:ENC-CALL-REL32   s" e833ffffff" X=
   \ llvm-objdump: 7c f4  jl  (10 bytes back)
   X64ASM:C-L -12 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8   s" 7cf4" X=
   \ llvm-objdump: 0f 85 32 ff ff ff  jne  (200 bytes back)
   X64ASM:C-NE -206 X64ASM:>REL SINK X64ASM:ENC-JCC-REL32   s" 0f8532ffffff" X= ;

\ A rejected operand must also leave the sink untouched: every encoder screens
\ every operand before its first byte, so a rejection cannot deposit the prefix of
\ an instruction that was never emitted.
: REGISTER-REJECTIONS ( -- )
   [: 16 X64ASM:>R64 SINK X64ASM:ENC-PUSH ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: -1 X64ASM:>R64 SINK X64ASM:ENC-NEG ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: 16 X64ASM:>R8 0 X64ASM:>R8 SINK X64ASM:ENC-MOV8-RR ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: 0 X64ASM:>R16 16 X64ASM:>R16 SINK X64ASM:ENC-MOV16-RR ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: 0 X64ASM:>R32 -1 X64ASM:>R32 SINK X64ASM:ENC-XOR32-RR ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T= ;

: IMMEDIATE-REJECTIONS ( -- )
   [: X64ASM:RAX 128 X64ASM:>IMM8 SINK X64ASM:ENC-ADD-RI8 ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: X64ASM:RAX -129 X64ASM:>IMM8 SINK X64ASM:ENC-ADD-RI8 ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: X64ASM:RAX 2147483648 X64ASM:>IMM32 SINK X64ASM:ENC-MOV-RI32 ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: X64ASM:RAX -2147483649 X64ASM:>IMM32 SINK X64ASM:ENC-MOV-RI32 ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   \ B8+rd zero-extends, so its immediate is unsigned where C7 /0's is signed.
   [: 0 X64ASM:>R32 -1 X64ASM:>IMM32 SINK X64ASM:ENC-MOV32-RI32 ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: 0 X64ASM:>R32 4294967296 X64ASM:>IMM32 SINK X64ASM:ENC-MOV32-RI32 ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: X64ASM:RAX 64 X64ASM:>IMM8 SINK X64ASM:ENC-SHL-RI8 ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: X64ASM:RAX -1 X64ASM:>IMM8 SINK X64ASM:ENC-SAR-RI8 ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T= ;

: MEMORY-REJECTIONS ( -- )
   \ rsp is the "no index" encoding, so it can never BE an index.
   [: X64ASM:RAX X64ASM:RSP 4 0 X64ASM:MEM-IDX drop ;] X64ASM:E-OPERAND TTHROWSQ
   [: X64ASM:RAX X64ASM:RBX 3 0 X64ASM:MEM-IDX drop ;] X64ASM:E-OPERAND TTHROWSQ
   [: X64ASM:RAX X64ASM:RBX 0 0 X64ASM:MEM-IDX drop ;] X64ASM:E-OPERAND TTHROWSQ
   [: X64ASM:RAX 2147483648 X64ASM:MEM-OFF drop ;] X64ASM:E-OPERAND TTHROWSQ
   [: -2147483649 X64ASM:MEM-RIP drop ;] X64ASM:E-OPERAND TTHROWSQ ;

: BRANCH-REJECTIONS ( -- )
   [: 128 X64ASM:>REL SINK X64ASM:ENC-JMP-REL8 ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: -129 X64ASM:>REL SINK X64ASM:ENC-JMP-REL8 ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: 2147483648 X64ASM:>REL SINK X64ASM:ENC-JMP-REL32 ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: 16 X64ASM:>CONDITION 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL8 ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: -1 X64ASM:>CONDITION 0 X64ASM:>REL SINK X64ASM:ENC-JCC-REL32 ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: 16 X64ASM:>CONDITION 0 X64ASM:>R8 SINK X64ASM:ENC-SETCC ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T= ;

\ The lock prefix is the first byte, so a rejected operand must stop it too.
: LOCK-REJECTIONS ( -- )
   [: 16 X64ASM:>R64 X64ASM:RCX X64ASM:MEM-AT SINK X64ASM:ENC-LOCK-CMPXCHG-MR ;] X64ASM:E-OPERAND TTHROWSQ   SINK-LEN 0 T= ;

\ The operand types themselves: an ill-formed instruction is a CHECKER rejection,
\ not a runtime throw. -1 is accepted, 0 rejected.
: SSE-NOMINAL-CASES ( -- )
   s" X64-GOOD-SSE ( X64ASM:xmm X64ASM:xmm ^VEC:vector<u8> -- ) X64ASM:ENC-ADDSD-RR"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" X64-GOOD-CVT ( X64ASM:xmm X64ASM:r64 ^VEC:vector<u8> -- ) X64ASM:ENC-CVTSI2SD-RR"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" X64-BAD-SSE ( X64ASM:r64 X64ASM:xmm ^VEC:vector<u8> -- ) X64ASM:ENC-MOVSD-RR"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" X64-BAD-CVT ( X64ASM:xmm X64ASM:xmm ^VEC:vector<u8> -- ) X64ASM:ENC-CVTSI2SD-RR"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" X64-BAD-CVTT ( X64ASM:r32 X64ASM:xmm ^VEC:vector<u8> -- ) X64ASM:ENC-CVTTSD2SI-RR"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" X64-GOOD-MOVQ ( X64ASM:xmm X64ASM:r64 ^VEC:vector<u8> -- ) X64ASM:ENC-MOVQ-XR"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ the direction is the type: a general register first is the other form
   s" X64-BAD-MOVQ ( X64ASM:xmm X64ASM:r64 ^VEC:vector<u8> -- ) X64ASM:ENC-MOVQ-RX"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: NOMINAL-CASES ( -- )
   s" X64-GOOD-MOV ( X64ASM:r64 X64ASM:r64 ^VEC:vector<u8> -- ) X64ASM:ENC-MOV-RR"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" X64-GOOD-LOAD ( X64ASM:r64 X64ASM:mem ^VEC:vector<u8> -- ) X64ASM:ENC-MOV-RM"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" X64-GOOD-SET ( X64ASM:condition X64ASM:r8 ^VEC:vector<u8> -- ) X64ASM:ENC-SETCC"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ a 32-bit register where a 64-bit one is required
   s" X64-BAD-WIDTH ( X64ASM:r32 X64ASM:r64 ^VEC:vector<u8> -- ) X64ASM:ENC-MOV-RR"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ an imm64 where only an imm32 is encodable
   s" X64-BAD-IMM64 ( X64ASM:r64 X64ASM:imm64 ^VEC:vector<u8> -- ) X64ASM:ENC-MOV-RI32"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ an imm32 where the relocatable literal needs an imm64
   s" X64-BAD-IMM32 ( X64ASM:r64 X64ASM:imm32 ^VEC:vector<u8> -- ) X64ASM:ENC-MOV-RI64"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ a bare cell where a register is required
   s" X64-BAD-RAW ( n n ^VEC:vector<u8> -- ) X64ASM:ENC-MOV-RR"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ a byte register where a 16-bit one is required
   s" X64-BAD-EXT ( X64ASM:r64 X64ASM:r8 ^VEC:vector<u8> -- ) X64ASM:ENC-MOVZX-16-RR"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ a memory operand where a register is required
   s" X64-BAD-MEM ( X64ASM:r64 X64ASM:mem ^VEC:vector<u8> -- ) X64ASM:ENC-MOV-RR"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ a displacement where a condition is required
   s" X64-BAD-COND ( X64ASM:rel X64ASM:rel ^VEC:vector<u8> -- ) X64ASM:ENC-JCC-REL8"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ an imm8 where a shift by cl takes no immediate at all
   s" X64-BAD-SHIFT ( X64ASM:r64 X64ASM:imm8 ^VEC:vector<u8> -- ) X64ASM:ENC-SHL-CL"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ The relocation writer patches the imm64 at this offset; the movabs case above
\ is ten bytes long, so the immediate is its last eight.
: RELOCATION-CONTRACT ( -- )
   X64ASM:MOV-RI64-IMM-OFF 2 T=
   X64ASM:R11 $1122334455667788 X64ASM:>IMM64 SINK X64ASM:ENC-MOV-RI64
   SINK-LEN X64ASM:MOV-RI64-IMM-OFF 8 + T=
   SINK 0 VEC:RESIZE ;

: RUN ( -- )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW SINK-CELL !
   ALU-ADD-OR ALU-ADC-SBB ALU-AND-SUB ALU-XOR-CMP TEST-FORMS
   MOVES WIDENING ADDRESS-AND-ZERO ONE-REGISTER SIGNED-MULTIPLY
   IMMEDIATE-STORES CELL-STORES STORE-REJECTIONS
   PADDING-AND-TRAP
   SHIFTS CONTROL ATOMICS MEMORY-TRAPS CONDITION-NAMES REGISTER-NAMES BRANCHES
   SSE-REGISTERS SSE-MOVES SSE-ARITHMETIC SSE-PACKED SHA-ROUNDS
   SSE-CROSS SSE-REJECTIONS SSE-NOMINAL-CASES
   REGISTER-REJECTIONS IMMEDIATE-REJECTIONS MEMORY-REJECTIONS BRANCH-REJECTIONS
   LOCK-REJECTIONS NOMINAL-CASES RELOCATION-CONTRACT
   SINK VEC:DELETE
   T-REPORT ;

RUN
;package
