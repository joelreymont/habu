\ x86-64-asm.f — the x86_64 encoder suite (package X64ASM, src/arch/x86-64/asm.f).
\
\ Pure Habu: no assembler runs at test time. Every form is pinned to a fixed
\ expected byte string, and the `llvm-mc:` comment above each case is the source
\ line that produced it, so the suite documents where its numbers came from.
\ The strings were produced on 2026-09-17 with
\ `llvm-mc -triple=x86_64 -show-encoding` (LLVM 22.1.8), and the five relative
\ branches were read back out of an object file with
\ `llvm-mc -filetype=obj` plus `llvm-objdump -d --triple=x86_64`, because
\ llvm-mc leaves a fixup rather than bytes for a symbolic branch target.
\
\ WHY SOME CASES AVOID rax AND A SHIFT COUNT OF ONE. llvm-mc prefers the
\ accumulator short forms (05 id, A9 id, 90+r) and the D1 /n shift-by-one form.
\ src/arch/x86-64/asm.f implements exactly one encoding per operation and none
\ of those optimisations, so an immediate case names rcx, rbx or an extended
\ register and a shift case uses a count other than one; the encoder and
\ llvm-mc then agree on the general form.

require lib/test.f
require lib/byte-buffer.f
require test/checker-assert.f
require src/arch/x86-64/asm.f

\ The span reader answers a role; a byte-count assertion takes a raw cell.
\ Projection out of a cell family needs no ownership, so no reopen of NUM
\ (lib/byte-buffer-test.f takes the same step for the same reason).
CAST: X64T-BL>RAW ( NUM:byte-len -- n )

package X64ASM-TEST
private
using X64ASM

\ One sink for the whole suite: each case encodes into it, is compared, and
\ clears it, so a case can never read a neighbour's bytes.
create SINK BUF:HDR-BYTES allot

: N>BLEN ( n -- NUM:byte-len )
   NUM:BYTE-LEN
   MATCH NUM:numeric-result
      ok OF ENDOF                                negative OF E-BUF-BOUNDS throw ENDOF
      zero OF E-BUF-BOUNDS throw ENDOF           overflow OF E-BUF-BOUNDS throw ENDOF
      underflow OF E-BUF-BOUNDS throw ENDOF      bad-alignment OF E-BUF-BOUNDS throw ENDOF
      misaligned OF E-BUF-BOUNDS throw ENDOF
   ;MATCH ;

: SINK-LEN ( -- n )
   SINK BUF:SPAN$ X64T-BL>RAW {: a:ptr u:n :}  u ;

\ A malformed expected string is a defect in this suite's own data, so it dies
\ rather than comparing against a wrong byte. FMATH:E-DOMAIN is the tree's
\ existing "input outside this word's domain" code; the suite borrows it rather
\ than claiming a code of its own for one literal guard.
: HEX-DIGIT ( n -- n ) {: c:n :}
   c 48 >= c 57 <= and if c 48 - exit then
   c 97 >= c 102 <= and 0= if FMATH:E-DOMAIN throw then
   c 97 - 10 + ;

: HEX-BYTE ( ptr u8 n -- n ) {: a:ptr i:n :}
   a i 2 * + c@ HEX-DIGIT 4 lshift
   a i 2 * 1 + + c@ HEX-DIGIT or ;

: SPAN=HEX? ( ptr u8 n ptr u8 n -- bool ) {: da:ptr dlen:n ea:ptr eu:n :}
   eu 2 mod 0<> if FMATH:E-DOMAIN throw then
   dlen eu 2 / <> if false exit then
   dlen 0 ?do
      ea i HEX-BYTE da i + c@ <> if false unloop exit then
   loop
   true ;

\ Compare what the encoder just appended against the expected string, then empty
\ the sink for the next case.
: X= ( ptr u8 n -- ) {: ea:ptr eu:n :}
   SINK BUF:SPAN$ X64T-BL>RAW {: da:ptr dlen:n :}
   da dlen ea eu SPAN=HEX? TTRUE
   SINK BUF:CLEAR ;

\ add and or in all five forms: register to register, register
\ from memory, memory from register, sign-extended imm8 and imm32.
: ALU-ADD-OR ( -- )
   \ llvm-mc: addq %rbx, %rax
   RAX RBX SINK ENC-ADD-RR   s" 4801d8" X=
   \ llvm-mc: addq (%rcx), %rax
   RAX RCX MEM-AT SINK ENC-ADD-RM   s" 480301" X=
   \ llvm-mc: addq %rax, (%rcx)
   RAX RCX MEM-AT SINK ENC-ADD-MR   s" 480101" X=
   \ llvm-mc: addq $7, %rax
   RAX 7 >IMM8 SINK ENC-ADD-RI8   s" 4883c007" X=
   \ llvm-mc: addq $1000, %rcx
   RCX 1000 >IMM32 SINK ENC-ADD-RI32   s" 4881c1e8030000" X=
   \ llvm-mc: orq %r15, %r8
   R8 R15 SINK ENC-OR-RR   s" 4d09f8" X=
   \ llvm-mc: orq 16(%rsi), %rdx
   RDX RSI 16 MEM-OFF SINK ENC-OR-RM   s" 480b5610" X=
   \ llvm-mc: orq %rdx, 16(%rsi)
   RDX RSI 16 MEM-OFF SINK ENC-OR-MR   s" 48095610" X=
   \ llvm-mc: orq $-1, %rbx
   RBX -1 >IMM8 SINK ENC-OR-RI8   s" 4883cbff" X=
   \ llvm-mc: orq $2147483647, %rbx
   RBX 2147483647 >IMM32 SINK ENC-OR-RI32   s" 4881cbffffff7f" X= ;

\ adc and sbb. The r12 base needs a SIB byte with no index and
\ the r13 base needs a disp8 of zero; both also set REX.B.
: ALU-ADC-SBB ( -- )
   \ llvm-mc: adcq %rax, %rdx
   RDX RAX SINK ENC-ADC-RR   s" 4811c2" X=
   \ llvm-mc: adcq (%r12), %r9
   R9 R12 MEM-AT SINK ENC-ADC-RM   s" 4d130c24" X=
   \ llvm-mc: adcq %r9, (%r12)
   R9 R12 MEM-AT SINK ENC-ADC-MR   s" 4d110c24" X=
   \ llvm-mc: adcq $0, %r10
   R10 0 >IMM8 SINK ENC-ADC-RI8   s" 4983d200" X=
   \ llvm-mc: adcq $-2147483648, %r10
   R10 -2147483648 >IMM32 SINK ENC-ADC-RI32   s" 4981d200000080" X=
   \ llvm-mc: sbbq %rcx, %rdx
   RDX RCX SINK ENC-SBB-RR   s" 4819ca" X=
   \ llvm-mc: sbbq (%r13), %rbp
   RBP R13 MEM-AT SINK ENC-SBB-RM   s" 491b6d00" X=
   \ llvm-mc: sbbq %rbp, (%r13)
   RBP R13 MEM-AT SINK ENC-SBB-MR   s" 49196d00" X=
   \ llvm-mc: sbbq $127, %rsp
   RSP 127 >IMM8 SINK ENC-SBB-RI8   s" 4883dc7f" X=
   \ llvm-mc: sbbq $65536, %rsi
   RSI 65536 >IMM32 SINK ENC-SBB-RI32   s" 4881de00000100" X= ;

\ and and sub. The rsp base is the other SIB-only base, and the
\ scaled-index form carries its own SIB.
: ALU-AND-SUB ( -- )
   \ llvm-mc: andq %rsi, %rdi
   RDI RSI SINK ENC-AND-RR   s" 4821f7" X=
   \ llvm-mc: andq (%rsp), %rdi
   RDI RSP MEM-AT SINK ENC-AND-RM   s" 48233c24" X=
   \ llvm-mc: andq %rdi, (%rsp)
   RDI RSP MEM-AT SINK ENC-AND-MR   s" 48213c24" X=
   \ llvm-mc: andq $-128, %rdi
   RDI -128 >IMM8 SINK ENC-AND-RI8   s" 4883e780" X=
   \ llvm-mc: andq $4096, %rdi
   RDI 4096 >IMM32 SINK ENC-AND-RI32   s" 4881e700100000" X=
   \ llvm-mc: subq %r13, %r14
   R14 R13 SINK ENC-SUB-RR   s" 4d29ee" X=
   \ llvm-mc: subq 8(%rbx,%rcx,4), %rax
   RAX RBX RCX 4 8 MEM-IDX SINK ENC-SUB-RM   s" 482b448b08" X=
   \ llvm-mc: subq %rax, 8(%rbx,%rcx,4)
   RAX RBX RCX 4 8 MEM-IDX SINK ENC-SUB-MR   s" 4829448b08" X=
   \ llvm-mc: subq $1, %rbp
   RBP 1 >IMM8 SINK ENC-SUB-RI8   s" 4883ed01" X=
   \ llvm-mc: subq $100000, %rbp
   RBP 100000 >IMM32 SINK ENC-SUB-RI32   s" 4881eda0860100" X= ;

\ xor and cmp, including the two rip-relative forms and the
\ disp8/disp32 boundary at -128 and -129.
: ALU-XOR-CMP ( -- )
   \ llvm-mc: xorq %rcx, %rcx
   RCX RCX SINK ENC-XOR-RR   s" 4831c9" X=
   \ llvm-mc: xorq (%rip), %r11
   R11 0 MEM-RIP SINK ENC-XOR-RM   s" 4c331d00000000" X=
   \ llvm-mc: xorq %r11, 16(%rip)
   R11 16 MEM-RIP SINK ENC-XOR-MR   s" 4c311d10000000" X=
   \ llvm-mc: xorq $-16, %r15
   R15 -16 >IMM8 SINK ENC-XOR-RI8   s" 4983f7f0" X=
   \ llvm-mc: xorq $65536, %r15
   R15 65536 >IMM32 SINK ENC-XOR-RI32   s" 4981f700000100" X=
   \ llvm-mc: cmpq %r15, %r14
   R14 R15 SINK ENC-CMP-RR   s" 4d39fe" X=
   \ llvm-mc: cmpq -129(%rbx), %rax
   RAX RBX -129 MEM-OFF SINK ENC-CMP-RM   s" 483b837fffffff" X=
   \ llvm-mc: cmpq %rax, -128(%rbx)
   RAX RBX -128 MEM-OFF SINK ENC-CMP-MR   s" 48394380" X=
   \ llvm-mc: cmpq $8, %rcx
   RCX 8 >IMM8 SINK ENC-CMP-RI8   s" 4883f908" X=
   \ llvm-mc: cmpq $1048576, %rcx
   RCX 1048576 >IMM32 SINK ENC-CMP-RI32   s" 4881f900001000" X= ;

\ test has no sign-extended imm8 form, so it has three.
: TEST-FORMS ( -- )
   \ llvm-mc: testq %rbx, %rax
   RAX RBX SINK ENC-TEST-RR   s" 4885d8" X=
   \ llvm-mc: testq %rcx, (%rdi)
   RCX RDI MEM-AT SINK ENC-TEST-MR   s" 48850f" X=
   \ llvm-mc: testq $16, %rcx
   RCX 16 >IMM32 SINK ENC-TEST-RI32   s" 48f7c110000000" X= ;

\ moves, loads and stores at all four widths, and the three
\ constant forms. A 32-bit move zero-extends into the full register; movabs is
\ the relocatable literal whose immediate starts at MOV-RI64-IMM-OFF.
: MOVES ( -- )
   \ llvm-mc: movq %rbx, %rax
   RAX RBX SINK ENC-MOV-RR   s" 4889d8" X=
   \ llvm-mc: movq 8(%r13,%rcx,4), %r9
   R9 R13 RCX 4 8 MEM-IDX SINK ENC-MOV-RM   s" 4d8b4c8d08" X=
   \ llvm-mc: movq %r13, 24(%r12,%r15,8)
   R13 R12 R15 8 24 MEM-IDX SINK ENC-MOV-MR   s" 4f896cfc18" X=
   \ llvm-mc: movl %ebx, %eax
   0 >R32 3 >R32 SINK ENC-MOV32-RR   s" 89d8" X=
   \ llvm-mc: movl (%rdi), %eax
   0 >R32 RDI MEM-AT SINK ENC-MOV32-RM   s" 8b07" X=
   \ llvm-mc: movl %eax, (%rdi)
   0 >R32 RDI MEM-AT SINK ENC-MOV32-MR   s" 8907" X=
   \ llvm-mc: movw %ax, %bx
   3 >R16 0 >R16 SINK ENC-MOV16-RR   s" 6689c3" X=
   \ llvm-mc: movw (%rdi), %ax
   0 >R16 RDI MEM-AT SINK ENC-MOV16-RM   s" 668b07" X=
   \ llvm-mc: movw %ax, (%rdi)
   0 >R16 RDI MEM-AT SINK ENC-MOV16-MR   s" 668907" X=
   \ llvm-mc: movb %al, %sil
   6 >R8 0 >R8 SINK ENC-MOV8-RR   s" 4088c6" X=
   \ llvm-mc: movb (%rdi), %sil
   6 >R8 RDI MEM-AT SINK ENC-MOV8-RM   s" 408a37" X=
   \ llvm-mc: movb %sil, (%rdi)
   6 >R8 RDI MEM-AT SINK ENC-MOV8-MR   s" 408837" X=
   \ llvm-mc: movq $-1, %rax
   RAX -1 >IMM32 SINK ENC-MOV-RI32   s" 48c7c0ffffffff" X=
   \ llvm-mc: movl $4294967295, %eax
   0 >R32 4294967295 >IMM32 SINK ENC-MOV32-RI32   s" b8ffffffff" X=
   \ llvm-mc: movabsq $1234605616436508552, %r11
   R11 $1122334455667788 >IMM64 SINK ENC-MOV-RI64   s" 49bb8877665544332211" X= ;

\ movzx, movsx and movsxd. A byte source at or above 4 is
\ sil/dil and carries REX even when no REX field is set.
: WIDENING ( -- )
   \ llvm-mc: movzbq %sil, %rax
   RAX 6 >R8 SINK ENC-MOVZX-8-RR   s" 480fb6c6" X=
   \ llvm-mc: movzbq (%rdi), %rax
   RAX RDI MEM-AT SINK ENC-MOVZX-8-RM   s" 480fb607" X=
   \ llvm-mc: movzwq %si, %rax
   RAX 6 >R16 SINK ENC-MOVZX-16-RR   s" 480fb7c6" X=
   \ llvm-mc: movzwq (%rdi), %rax
   RAX RDI MEM-AT SINK ENC-MOVZX-16-RM   s" 480fb707" X=
   \ llvm-mc: movsbq %dil, %rax
   RAX 7 >R8 SINK ENC-MOVSX-8-RR   s" 480fbec7" X=
   \ llvm-mc: movsbq (%rdi), %rax
   RAX RDI MEM-AT SINK ENC-MOVSX-8-RM   s" 480fbe07" X=
   \ llvm-mc: movswq %si, %rax
   RAX 6 >R16 SINK ENC-MOVSX-16-RR   s" 480fbfc6" X=
   \ llvm-mc: movswq (%rdi), %rax
   RAX RDI MEM-AT SINK ENC-MOVSX-16-RM   s" 480fbf07" X=
   \ llvm-mc: movslq %ecx, %rax
   RAX 1 >R32 SINK ENC-MOVSXD-RR   s" 4863c1" X=
   \ llvm-mc: movslq (%rdi), %rax
   RAX RDI MEM-AT SINK ENC-MOVSXD-RM   s" 486307" X= ;

\ lea and the 32-bit zeroing idiom.
: ADDRESS-AND-ZERO ( -- )
   \ llvm-mc: leaq 16(%rax,%rbx,8), %rcx
   RCX RAX RBX 8 16 MEM-IDX SINK ENC-LEA   s" 488d4cd810" X=
   \ llvm-mc: xorl %eax, %eax
   0 >R32 0 >R32 SINK ENC-XOR32-RR   s" 31c0" X= ;

\ the F7 and FF groups and cqo.
: ONE-REGISTER ( -- )
   \ llvm-mc: notq %r15
   R15 SINK ENC-NOT   s" 49f7d7" X=
   \ llvm-mc: negq %rax
   RAX SINK ENC-NEG   s" 48f7d8" X=
   \ llvm-mc: mulq %rcx
   RCX SINK ENC-MUL   s" 48f7e1" X=
   \ llvm-mc: imulq %rcx
   RCX SINK ENC-IMUL1   s" 48f7e9" X=
   \ llvm-mc: divq %rcx
   RCX SINK ENC-DIV   s" 48f7f1" X=
   \ llvm-mc: idivq %rcx
   RCX SINK ENC-IDIV   s" 48f7f9" X=
   \ llvm-mc: incq %rax
   RAX SINK ENC-INC   s" 48ffc0" X=
   \ llvm-mc: decq %r8
   R8 SINK ENC-DEC   s" 49ffc8" X=
   \ llvm-mc: cqto
   SINK ENC-CQO   s" 4899" X= ;

\ the non-widening imul in its four forms.
: SIGNED-MULTIPLY ( -- )
   \ llvm-mc: imulq %rcx, %rax
   RAX RCX SINK ENC-IMUL-RR   s" 480fafc1" X=
   \ llvm-mc: imulq (%rdi), %rax
   RAX RDI MEM-AT SINK ENC-IMUL-RM   s" 480faf07" X=
   \ llvm-mc: imulq $3, %rcx, %rax
   RAX RCX 3 >IMM8 SINK ENC-IMUL-RRI8   s" 486bc103" X=
   \ llvm-mc: imulq $300, %rcx, %rax
   RAX RCX 300 >IMM32 SINK ENC-IMUL-RRI32   s" 4869c12c010000" X= ;

\ shifts and rotates by imm8 and by cl.
: SHIFTS ( -- )
   \ llvm-mc: rolq $4, %rax
   RAX 4 >IMM8 SINK ENC-ROL-RI8   s" 48c1c004" X=
   \ llvm-mc: rorq $2, %rbx
   RBX 2 >IMM8 SINK ENC-ROR-RI8   s" 48c1cb02" X=
   \ llvm-mc: shlq $3, %rax
   RAX 3 >IMM8 SINK ENC-SHL-RI8   s" 48c1e003" X=
   \ llvm-mc: shrq $63, %rax
   RAX 63 >IMM8 SINK ENC-SHR-RI8   s" 48c1e83f" X=
   \ llvm-mc: sarq $2, %r9
   R9 2 >IMM8 SINK ENC-SAR-RI8   s" 49c1f902" X=
   \ llvm-mc: rolq %cl, %rax
   RAX SINK ENC-ROL-CL   s" 48d3c0" X=
   \ llvm-mc: rorq %cl, %rbx
   RBX SINK ENC-ROR-CL   s" 48d3cb" X=
   \ llvm-mc: shlq %cl, %r10
   R10 SINK ENC-SHL-CL   s" 49d3e2" X=
   \ llvm-mc: shrq %cl, %rax
   RAX SINK ENC-SHR-CL   s" 48d3e8" X=
   \ llvm-mc: sarq %cl, %rax
   RAX SINK ENC-SAR-CL   s" 48d3f8" X= ;

\ indirect branches, return, syscall, setcc, cmovcc, the stack
\ and the exchange.
: CONTROL ( -- )
   \ llvm-mc: jmpq *%rax
   RAX SINK ENC-JMP-REG   s" ffe0" X=
   \ llvm-mc: jmpq *%r11
   R11 SINK ENC-JMP-REG   s" 41ffe3" X=
   \ llvm-mc: callq *%rax
   RAX SINK ENC-CALL-REG   s" ffd0" X=
   \ llvm-mc: ret
   SINK ENC-RET   s" c3" X=
   \ llvm-mc: syscall
   SINK ENC-SYSCALL   s" 0f05" X=
   \ llvm-mc: setl %al
   C-L 0 >R8 SINK ENC-SETCC   s" 0f9cc0" X=
   \ llvm-mc: setl %sil
   C-L 6 >R8 SINK ENC-SETCC   s" 400f9cc6" X=
   \ llvm-mc: cmovlq %rcx, %rax
   C-L RAX RCX SINK ENC-CMOVCC   s" 480f4cc1" X=
   \ llvm-mc: pushq %rax
   RAX SINK ENC-PUSH   s" 50" X=
   \ llvm-mc: pushq %r13
   R13 SINK ENC-PUSH   s" 4155" X=
   \ llvm-mc: popq %rbp
   RBP SINK ENC-POP   s" 5d" X=
   \ llvm-mc: popq %r12
   R12 SINK ENC-POP   s" 415c" X=
   \ llvm-mc: xchgq %rbx, %rcx
   RCX RBX SINK ENC-XCHG-RR   s" 4887cb" X= ;

\ the addressing forms that have no ARM counterpart: rsp and
\ r12 forcing a SIB, rbp and r13 forcing a disp8 of zero, the disp8 boundary,
\ rip-relative, and an rsp base carrying an index.
: MEMORY-TRAPS ( -- )
   \ llvm-mc: movq (%rsp), %rax
   RAX RSP MEM-AT SINK ENC-MOV-RM   s" 488b0424" X=
   \ llvm-mc: movq (%rbp), %rax
   RAX RBP MEM-AT SINK ENC-MOV-RM   s" 488b4500" X=
   \ llvm-mc: movq (%r12), %rax
   RAX R12 MEM-AT SINK ENC-MOV-RM   s" 498b0424" X=
   \ llvm-mc: movq (%r13), %rax
   RAX R13 MEM-AT SINK ENC-MOV-RM   s" 498b4500" X=
   \ llvm-mc: movq -128(%rbx), %rax
   RAX RBX -128 MEM-OFF SINK ENC-MOV-RM   s" 488b4380" X=
   \ llvm-mc: movq -129(%rbx), %rax
   RAX RBX -129 MEM-OFF SINK ENC-MOV-RM   s" 488b837fffffff" X=
   \ llvm-mc: movq (%rip), %rax
   RAX 0 MEM-RIP SINK ENC-MOV-RM   s" 488b0500000000" X=
   \ llvm-mc: movq 16(%rip), %rax
   RAX 16 MEM-RIP SINK ENC-MOV-RM   s" 488b0510000000" X=
   \ llvm-mc: movq %r15, 4(%rsp,%rbp,2)
   R15 RSP RBP 2 4 MEM-IDX SINK ENC-MOV-MR   s" 4c897c6c04" X= ;
\ The sixteen condition names, each through the one-byte conditional jump, so a
\ transposed code cannot hide: the opcode IS 70 plus the condition.
: CONDITION-NAMES ( -- )
   \ llvm-objdump: jo (to the next instruction)
   C-O 0 >REL SINK ENC-JCC-REL8   s" 7000" X=
   \ llvm-objdump: jno (to the next instruction)
   C-NO 0 >REL SINK ENC-JCC-REL8   s" 7100" X=
   \ llvm-objdump: jb (to the next instruction)
   C-B 0 >REL SINK ENC-JCC-REL8   s" 7200" X=
   \ llvm-objdump: jae (to the next instruction)
   C-AE 0 >REL SINK ENC-JCC-REL8   s" 7300" X=
   \ llvm-objdump: je (to the next instruction)
   C-E 0 >REL SINK ENC-JCC-REL8   s" 7400" X=
   \ llvm-objdump: jne (to the next instruction)
   C-NE 0 >REL SINK ENC-JCC-REL8   s" 7500" X=
   \ llvm-objdump: jbe (to the next instruction)
   C-BE 0 >REL SINK ENC-JCC-REL8   s" 7600" X=
   \ llvm-objdump: ja (to the next instruction)
   C-A 0 >REL SINK ENC-JCC-REL8   s" 7700" X=
   \ llvm-objdump: js (to the next instruction)
   C-S 0 >REL SINK ENC-JCC-REL8   s" 7800" X=
   \ llvm-objdump: jns (to the next instruction)
   C-NS 0 >REL SINK ENC-JCC-REL8   s" 7900" X=
   \ llvm-objdump: jp (to the next instruction)
   C-P 0 >REL SINK ENC-JCC-REL8   s" 7a00" X=
   \ llvm-objdump: jnp (to the next instruction)
   C-NP 0 >REL SINK ENC-JCC-REL8   s" 7b00" X=
   \ llvm-objdump: jl (to the next instruction)
   C-L 0 >REL SINK ENC-JCC-REL8   s" 7c00" X=
   \ llvm-objdump: jge (to the next instruction)
   C-GE 0 >REL SINK ENC-JCC-REL8   s" 7d00" X=
   \ llvm-objdump: jle (to the next instruction)
   C-LE 0 >REL SINK ENC-JCC-REL8   s" 7e00" X=
   \ llvm-objdump: jg (to the next instruction)
   C-G 0 >REL SINK ENC-JCC-REL8   s" 7f00" X= ;

\ The sixteen register names, each through push, so a misnumbered name cannot
\ hide: the opcode IS 50 plus the low three bits, with REX.B above seven.
: REGISTER-NAMES ( -- )
   \ llvm-objdump: pushq %rax
   RAX SINK ENC-PUSH   s" 50" X=
   \ llvm-objdump: pushq %rcx
   RCX SINK ENC-PUSH   s" 51" X=
   \ llvm-objdump: pushq %rdx
   RDX SINK ENC-PUSH   s" 52" X=
   \ llvm-objdump: pushq %rbx
   RBX SINK ENC-PUSH   s" 53" X=
   \ llvm-objdump: pushq %rsp
   RSP SINK ENC-PUSH   s" 54" X=
   \ llvm-objdump: pushq %rbp
   RBP SINK ENC-PUSH   s" 55" X=
   \ llvm-objdump: pushq %rsi
   RSI SINK ENC-PUSH   s" 56" X=
   \ llvm-objdump: pushq %rdi
   RDI SINK ENC-PUSH   s" 57" X=
   \ llvm-objdump: pushq %r8
   R8 SINK ENC-PUSH   s" 4150" X=
   \ llvm-objdump: pushq %r9
   R9 SINK ENC-PUSH   s" 4151" X=
   \ llvm-objdump: pushq %r10
   R10 SINK ENC-PUSH   s" 4152" X=
   \ llvm-objdump: pushq %r11
   R11 SINK ENC-PUSH   s" 4153" X=
   \ llvm-objdump: pushq %r12
   R12 SINK ENC-PUSH   s" 4154" X=
   \ llvm-objdump: pushq %r13
   R13 SINK ENC-PUSH   s" 4155" X=
   \ llvm-objdump: pushq %r14
   R14 SINK ENC-PUSH   s" 4156" X=
   \ llvm-objdump: pushq %r15
   R15 SINK ENC-PUSH   s" 4157" X= ;

: SSE-REGISTERS ( -- )
   \ llvm-mc: movsd %xmm0, %xmm0
   XMM0 XMM0 SINK ENC-MOVSD-RR   s" f20f10c0" X=
   \ llvm-mc: movsd %xmm0, %xmm1
   XMM1 XMM0 SINK ENC-MOVSD-RR   s" f20f10c8" X=
   \ llvm-mc: movsd %xmm0, %xmm2
   XMM2 XMM0 SINK ENC-MOVSD-RR   s" f20f10d0" X=
   \ llvm-mc: movsd %xmm0, %xmm3
   XMM3 XMM0 SINK ENC-MOVSD-RR   s" f20f10d8" X=
   \ llvm-mc: movsd %xmm0, %xmm4
   XMM4 XMM0 SINK ENC-MOVSD-RR   s" f20f10e0" X=
   \ llvm-mc: movsd %xmm0, %xmm5
   XMM5 XMM0 SINK ENC-MOVSD-RR   s" f20f10e8" X=
   \ llvm-mc: movsd %xmm0, %xmm6
   XMM6 XMM0 SINK ENC-MOVSD-RR   s" f20f10f0" X=
   \ llvm-mc: movsd %xmm0, %xmm7
   XMM7 XMM0 SINK ENC-MOVSD-RR   s" f20f10f8" X=
   \ llvm-mc: movsd %xmm0, %xmm8
   XMM8 XMM0 SINK ENC-MOVSD-RR   s" f2440f10c0" X=
   \ llvm-mc: movsd %xmm0, %xmm9
   XMM9 XMM0 SINK ENC-MOVSD-RR   s" f2440f10c8" X=
   \ llvm-mc: movsd %xmm0, %xmm10
   XMM10 XMM0 SINK ENC-MOVSD-RR   s" f2440f10d0" X=
   \ llvm-mc: movsd %xmm0, %xmm11
   XMM11 XMM0 SINK ENC-MOVSD-RR   s" f2440f10d8" X=
   \ llvm-mc: movsd %xmm0, %xmm12
   XMM12 XMM0 SINK ENC-MOVSD-RR   s" f2440f10e0" X=
   \ llvm-mc: movsd %xmm0, %xmm13
   XMM13 XMM0 SINK ENC-MOVSD-RR   s" f2440f10e8" X=
   \ llvm-mc: movsd %xmm0, %xmm14
   XMM14 XMM0 SINK ENC-MOVSD-RR   s" f2440f10f0" X=
   \ llvm-mc: movsd %xmm0, %xmm15
   XMM15 XMM0 SINK ENC-MOVSD-RR   s" f2440f10f8" X= ;

: SSE-MOVES ( -- )
   \ llvm-mc: movsd %xmm15, %xmm8
   XMM8 XMM15 SINK ENC-MOVSD-RR   s" f2450f10c7" X=
   \ llvm-mc: movsd %xmm15, %xmm0
   XMM0 XMM15 SINK ENC-MOVSD-RR   s" f2410f10c7" X=
   \ llvm-mc: movsd 8(%r13,%r12,4), %xmm9
   XMM9 R13 R12 4 8 MEM-IDX SINK ENC-MOVSD-RM   s" f2470f104ca508" X=
   \ llvm-mc: movsd %xmm15, -129(%r12)
   XMM15 R12 -129 MEM-OFF SINK ENC-MOVSD-MR   s" f2450f11bc247fffffff" X=
   \ llvm-mc: movsd 16(%rip), %xmm8
   XMM8 16 MEM-RIP SINK ENC-MOVSD-RM   s" f2440f100510000000" X=
   \ llvm-mc: movsd %xmm1, -16(%rip)
   XMM1 -16 MEM-RIP SINK ENC-MOVSD-MR   s" f20f110df0ffffff" X=
   \ llvm-mc: movsd (%rbp), %xmm0
   XMM0 RBP MEM-AT SINK ENC-MOVSD-RM   s" f20f104500" X=
   \ llvm-mc: movsd (%rsp), %xmm1
   XMM1 RSP MEM-AT SINK ENC-MOVSD-RM   s" f20f100c24" X= ;

: SSE-ARITHMETIC ( -- )
   \ llvm-mc: addsd %xmm9, %xmm8
   XMM8 XMM9 SINK ENC-ADDSD-RR   s" f2450f58c1" X=
   \ llvm-mc: subsd %xmm9, %xmm8
   XMM8 XMM9 SINK ENC-SUBSD-RR   s" f2450f5cc1" X=
   \ llvm-mc: mulsd %xmm9, %xmm8
   XMM8 XMM9 SINK ENC-MULSD-RR   s" f2450f59c1" X=
   \ llvm-mc: divsd %xmm9, %xmm8
   XMM8 XMM9 SINK ENC-DIVSD-RR   s" f2450f5ec1" X=
   \ llvm-mc: sqrtsd %xmm9, %xmm8
   XMM8 XMM9 SINK ENC-SQRTSD-RR   s" f2450f51c1" X=
   \ llvm-mc: andpd %xmm15, %xmm8
   XMM8 XMM15 SINK ENC-ANDPD-RR   s" 66450f54c7" X=
   \ llvm-mc: xorpd %xmm15, %xmm8
   XMM8 XMM15 SINK ENC-XORPD-RR   s" 66450f57c7" X=
   \ llvm-mc: ucomisd %xmm15, %xmm8
   XMM8 XMM15 SINK ENC-UCOMISD-RR   s" 66450f2ec7" X=
   \ llvm-mc: cvtsi2sdq %r15, %xmm8
   XMM8 R15 SINK ENC-CVTSI2SD-RR   s" f24d0f2ac7" X=
   \ llvm-mc: cvttsd2siq %xmm15, %r8
   R8 XMM15 SINK ENC-CVTTSD2SI-RR   s" f24d0f2cc7" X=
   \ llvm-mc: cvtsi2sdq %rax, %xmm0
   XMM0 RAX SINK ENC-CVTSI2SD-RR   s" f2480f2ac0" X=
   \ llvm-mc: cvttsd2siq %xmm1, %rcx
   RCX XMM1 SINK ENC-CVTTSD2SI-RR   s" f2480f2cc9" X= ;

: SSE-REFUSALS ( -- )
   \ Preserve an existing instruction as well as emitting no partial prefix.
   RAX SINK ENC-PUSH
   [: 16 >XMM XMM0 SINK ENC-MOVSD-RR ;] E-OPERAND TTHROWSQ
   [: XMM0 -1 >XMM SINK ENC-ADDSD-RR ;] E-OPERAND TTHROWSQ
   [: XMM0 16 >R64 SINK ENC-CVTSI2SD-RR ;] E-OPERAND TTHROWSQ
   [: -1 >R64 XMM0 SINK ENC-CVTTSD2SI-RR ;] E-OPERAND TTHROWSQ
   [: 16 >XMM RAX MEM-AT SINK ENC-MOVSD-RM ;] E-OPERAND TTHROWSQ
   [: -1 >XMM RAX MEM-AT SINK ENC-MOVSD-MR ;] E-OPERAND TTHROWSQ
   [: XMM0 16 32 lshift >MEM SINK ENC-MOVSD-RM ;] E-OPERAND TTHROWSQ
   [: XMM0 4 37 lshift >MEM SINK ENC-MOVSD-MR ;] E-OPERAND TTHROWSQ
   [: XMM0 1 44 lshift >MEM SINK ENC-MOVSD-RM ;] E-OPERAND TTHROWSQ
   s" 50" X= ;

\ The five relative branches. A displacement is measured from the END of the
\ instruction, so the -12 cases jump back over ten bytes plus their own two, and
\ the rel32 cases back over two hundred plus their own five or six. Bytes read
\ with llvm-objdump from an object llvm-mc assembled from `1: .space N` / `jl 1b`.
: BRANCHES ( -- )
   \ llvm-objdump: eb f4  jmp  (10 bytes back)
   -12 >REL SINK ENC-JMP-REL8   s" ebf4" X=
   \ llvm-objdump: e9 33 ff ff ff  jmp  (200 bytes back)
   -205 >REL SINK ENC-JMP-REL32   s" e933ffffff" X=
   \ llvm-objdump: e8 33 ff ff ff  callq  (200 bytes back)
   -205 >REL SINK ENC-CALL-REL32   s" e833ffffff" X=
   \ llvm-objdump: 7c f4  jl  (10 bytes back)
   C-L -12 >REL SINK ENC-JCC-REL8   s" 7cf4" X=
   \ llvm-objdump: 0f 85 32 ff ff ff  jne  (200 bytes back)
   C-NE -206 >REL SINK ENC-JCC-REL32   s" 0f8532ffffff" X= ;

\ A refused operand must also leave the sink untouched: every encoder screens
\ every operand before its first byte, so a refusal cannot deposit the prefix of
\ an instruction that was never emitted.
: REGISTER-REFUSALS ( -- )
   [: 16 >R64 SINK ENC-PUSH ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: -1 >R64 SINK ENC-NEG ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: 16 >R8 0 >R8 SINK ENC-MOV8-RR ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: 0 >R16 16 >R16 SINK ENC-MOV16-RR ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: 0 >R32 -1 >R32 SINK ENC-XOR32-RR ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T= ;

: IMMEDIATE-REFUSALS ( -- )
   [: RAX 128 >IMM8 SINK ENC-ADD-RI8 ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: RAX -129 >IMM8 SINK ENC-ADD-RI8 ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: RAX 2147483648 >IMM32 SINK ENC-MOV-RI32 ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: RAX -2147483649 >IMM32 SINK ENC-MOV-RI32 ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   \ B8+rd zero-extends, so its immediate is unsigned where C7 /0's is signed.
   [: 0 >R32 -1 >IMM32 SINK ENC-MOV32-RI32 ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: 0 >R32 4294967296 >IMM32 SINK ENC-MOV32-RI32 ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: RAX 64 >IMM8 SINK ENC-SHL-RI8 ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: RAX -1 >IMM8 SINK ENC-SAR-RI8 ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T= ;

: MEMORY-REFUSALS ( -- )
   \ rsp is the "no index" encoding, so it can never BE an index.
   [: RAX RSP 4 0 MEM-IDX drop ;] E-OPERAND TTHROWSQ
   [: RAX RBX 3 0 MEM-IDX drop ;] E-OPERAND TTHROWSQ
   [: RAX RBX 0 0 MEM-IDX drop ;] E-OPERAND TTHROWSQ
   [: RAX 2147483648 MEM-OFF drop ;] E-OPERAND TTHROWSQ
   [: -2147483649 MEM-RIP drop ;] E-OPERAND TTHROWSQ
   \ A memory operand forged through the generated cast is screened on the way in.
   [: RAX 16 32 lshift >MEM SINK ENC-MOV-RM ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: RAX 4 37 lshift >MEM SINK ENC-MOV-RM ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: RAX 1 44 lshift >MEM SINK ENC-MOV-RM ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T= ;

: BRANCH-REFUSALS ( -- )
   [: 128 >REL SINK ENC-JMP-REL8 ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: -129 >REL SINK ENC-JMP-REL8 ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: 2147483648 >REL SINK ENC-JMP-REL32 ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: 16 >CONDITION 0 >REL SINK ENC-JCC-REL8 ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: -1 >CONDITION 0 >REL SINK ENC-JCC-REL32 ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T=
   [: 16 >CONDITION 0 >R8 SINK ENC-SETCC ;] E-OPERAND TTHROWSQ   SINK-LEN 0 T= ;

\ The operand types themselves: an ill-formed instruction is a CHECKER refusal,
\ not a runtime throw. -1 is accepted, 0 refused.
: SSE-NOMINAL-CASES ( -- )
   s" X64-GOOD-SSE ( X64ASM:xmm X64ASM:xmm ptr a -- ) X64ASM:ENC-ADDSD-RR"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" X64-GOOD-CVT ( X64ASM:xmm X64ASM:r64 ptr a -- ) X64ASM:ENC-CVTSI2SD-RR"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" X64-BAD-SSE ( X64ASM:r64 X64ASM:xmm ptr a -- ) X64ASM:ENC-MOVSD-RR"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" X64-BAD-CVT ( X64ASM:xmm X64ASM:xmm ptr a -- ) X64ASM:ENC-CVTSI2SD-RR"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" X64-BAD-CVTT ( X64ASM:r32 X64ASM:xmm ptr a -- ) X64ASM:ENC-CVTTSD2SI-RR"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: NOMINAL-CASES ( -- )
   s" X64-GOOD-MOV ( X64ASM:r64 X64ASM:r64 ptr a -- ) X64ASM:ENC-MOV-RR"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" X64-GOOD-LOAD ( X64ASM:r64 X64ASM:mem ptr a -- ) X64ASM:ENC-MOV-RM"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" X64-GOOD-SET ( X64ASM:condition X64ASM:r8 ptr a -- ) X64ASM:ENC-SETCC"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ a 32-bit register where a 64-bit one is required
   s" X64-BAD-WIDTH ( X64ASM:r32 X64ASM:r64 ptr a -- ) X64ASM:ENC-MOV-RR"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ an imm64 where only an imm32 is encodable
   s" X64-BAD-IMM64 ( X64ASM:r64 X64ASM:imm64 ptr a -- ) X64ASM:ENC-MOV-RI32"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ an imm32 where the relocatable literal needs an imm64
   s" X64-BAD-IMM32 ( X64ASM:r64 X64ASM:imm32 ptr a -- ) X64ASM:ENC-MOV-RI64"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ a bare cell where a register is required
   s" X64-BAD-RAW ( n n ptr a -- ) X64ASM:ENC-MOV-RR"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ a byte register where a 16-bit one is required
   s" X64-BAD-EXT ( X64ASM:r64 X64ASM:r8 ptr a -- ) X64ASM:ENC-MOVZX-16-RR"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ a memory operand where a register is required
   s" X64-BAD-MEM ( X64ASM:r64 X64ASM:mem ptr a -- ) X64ASM:ENC-MOV-RR"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ a displacement where a condition is required
   s" X64-BAD-COND ( X64ASM:rel X64ASM:rel ptr a -- ) X64ASM:ENC-JCC-REL8"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ an imm8 where a shift by cl takes no immediate at all
   s" X64-BAD-SHIFT ( X64ASM:r64 X64ASM:imm8 ptr a -- ) X64ASM:ENC-SHL-CL"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ The relocation writer patches the imm64 at this offset; the movabs case above
\ is ten bytes long, so the immediate is its last eight.
: RELOCATION-CONTRACT ( -- )
   MOV-RI64-IMM-OFF 2 T=
   R11 $1122334455667788 >IMM64 SINK ENC-MOV-RI64
   SINK-LEN MOV-RI64-IMM-OFF 8 + T=
   SINK BUF:CLEAR ;

: RUN ( -- )
   SINK 64 N>BLEN BUF:INIT
   ALU-ADD-OR ALU-ADC-SBB ALU-AND-SUB ALU-XOR-CMP TEST-FORMS
   MOVES WIDENING ADDRESS-AND-ZERO ONE-REGISTER SIGNED-MULTIPLY
   SHIFTS CONTROL MEMORY-TRAPS CONDITION-NAMES REGISTER-NAMES BRANCHES
   SSE-REGISTERS SSE-MOVES SSE-ARITHMETIC SSE-REFUSALS SSE-NOMINAL-CASES
   REGISTER-REFUSALS IMMEDIATE-REFUSALS MEMORY-REFUSALS BRANCH-REFUSALS
   NOMINAL-CASES RELOCATION-CONTRACT
   SINK BUF:DISPOSE
   T-REPORT ;

RUN
;package
