\ rt.fs — native runtime routines for the ENGINE-BUILDER port. Emits stack and
\ printer instruction sequences via mnem.fs. Keep this stage-source file
\ local-free so the Gforth recovery compiler can check it.
\ The engine's data-stack register is stated twice: src/arch/arm64/mnem.f's XDS,
\ which every push and pop below emits through, and src/habu/layout.f's
\ ENGINE-GPR:DSTACK, which the compiler chain derives its reserved-register mask
\ from. The two cannot be one constant - mnem.f is build-side vocabulary the
\ booted engine never re-reads, layout.f is re-read at every load - so agreement
\ is EXECUTED here instead of assumed: both build chains (hb-build and the
\ Gforth recovery) load mnem.f, then layout.f, then this file, the first
\ consumer of XDS, and a build whose emitters and whose reserved mask disagree
\ about the stack register dies here instead of shipping an engine whose
\ allocator can be handed the register the stack lives in.
\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
require src/habu/stack-abi.f
using A64ASM

package RT

72 constant EXIT-RC

: DSTACK-AGREE ( -- )
   XDS ENGINE-GPR:DSTACK <>
   if s" rt: mnem.f XDS and layout.f ENGINE-GPR:DSTACK disagree" EXIT-RC die then ;
DSTACK-AGREE

;package

package STACK-GUARD

variable LDATA
variable LRETURN
variable LLOOP
variable LUNDER
variable REQ-BELOW
variable REQ-ABOVE
variable REQ-TARGET
variable FAIL-MESSAGE
variable DESCRIPTOR-FAIL
variable DESCRIPTOR-BASE
variable DESCRIPTOR-CAP
variable CURSOR-ABOVE
variable CHECK-FAIL
variable CHECK-UNDER
variable CHECK-END
variable CHECK-START
variable FIXED-OFF
variable FIXED-CAP

\ The wrapper preserves its argument registers and LR; the helpers preserve
\ x14/x15 and NZCV. Successful checks preserve every register and every flag.
: SAVE-CALLER ( -- )
   SP SP 32 SUBI,  16 SP 0 STR,  17 SP 8 STR,  30 SP 16 STR, ;

: RESTORE-CALLER ( -- )
   16 SP 0 LDR,  17 SP 8 LDR,  30 SP 16 LDR,  SP SP 32 ADDI, ;

: REQUEST ( n n label -- )
   REQ-TARGET !  REQ-ABOVE !  REQ-BELOW !
   SAVE-CALLER
   16 REQ-BELOW @ LIT64,  17 REQ-ABOVE @ LIT64,
   REQ-TARGET LABEL@ BL,
   RESTORE-CALLER ;

: SAVE-CHECK ( -- )
   SP SP 32 SUBI,  14 SP 0 STR,  15 SP 8 STR,
   $D53B420E EMITW  14 SP 16 STR, ;               \ mrs x14,NZCV

: RETURN-CHECK ( -- )
   14 SP 16 LDR,  $D51B420E EMITW                  \ msr NZCV,x14
   14 SP 0 LDR,  15 SP 8 LDR,  SP SP 32 ADDI,  RET, ;

: EMIT-FAIL ( -- )
   LBL FAIL-MESSAGE !
   0 2 MOVZ,  1 FAIL-MESSAGE LABEL@ ADR,  2 25 MOVZ,  NR-WRITE SYS,
   0 ENGINE-ERROR:STACK-BOUNDS MOVZ,  NR-EXIT-GROUP SYS,
   FAIL-MESSAGE LABEL@ LBL,  s" hb: stack bounds exceeded" BYTES, ;

\ The descriptor itself must not wrap, even
\ when it came from a saved frame or a task rather than run-in-stack.
: ALIGNED ( n label -- )
   DESCRIPTOR-FAIL !
   31 swap 7 >LIMM ENC-ANDI $60000000 or EMITW    \ tst register,#7
   C-NE DESCRIPTOR-FAIL LABEL@ BCOND, ;

: DESCRIPTOR ( n n label -- )
   DESCRIPTOR-FAIL !  DESCRIPTOR-CAP !  DESCRIPTOR-BASE !
   DESCRIPTOR-BASE @ DESCRIPTOR-FAIL LABEL@ CBZ,
   DESCRIPTOR-BASE @ DESCRIPTOR-FAIL LABEL@ ALIGNED
   DESCRIPTOR-BASE @ dup ENC-MVN EMITW
   DESCRIPTOR-CAP @ DESCRIPTOR-BASE @ CMP,  C-HI DESCRIPTOR-FAIL LABEL@ BCOND,
   DESCRIPTOR-BASE @ dup ENC-MVN EMITW ;

public

: LABELS ( -- )
   LBL LDATA !  LBL LRETURN !  LBL LLOOP !  LBL LUNDER ! ;

: DATA-ENTRY ( -- label ) LDATA LABEL@ ;
: RETURN-ENTRY ( -- label ) LRETURN LABEL@ ;
: LOOP-ENTRY ( -- label ) LLOOP LABEL@ ;

\ Where a data request below the base goes: the interpreter's E-UNDERFLOW
\ diagnostic, which habu2.f places under this label.
: UNDERFLOW-ENTRY ( -- label ) LUNDER LABEL@ ;

private

\ A request below the base is the one failure with a name. The interpreter's
\ E-UNDERFLOW diagnostic names the token whose execution underflowed, throws
\ RC-REJECT inside evaluate and recovers in the REPL, and it restores SP from
\ its own saved cells on both recovery legs, so the check releases only its
\ own frame before leaving for it. Every other failure - a push past the
\ capacity, a descriptor that does not describe a stack - has no token to
\ name and stays fail-closed here.
: EMIT-UNDERFLOW ( -- )
   SP SP 32 ADDI,  UNDERFLOW-ENTRY B, ;

public

\ The envelope is [XDS-below,XDS+above), with both distances in bytes.
\ It also validates pointer adjustments whose endpoint is XDS +/- distance.
: CHECK-DATA ( n n -- ) DATA-ENTRY REQUEST ;

\ Fixed stack envelopes use cell/frame counts relative to the saved depth.
: CHECK-RETURN ( n n -- ) RETURN-ENTRY REQUEST ;
: CHECK-LOOP ( n n -- ) LOOP-ENTRY REQUEST ;

\ Lifecycle admission: x14=base, x10=capacity, x12=cursor. Leaves base intact;
\ x10 becomes remaining bytes and x12 becomes used bytes. No memory is changed.
: CHECK-CURSOR ( n label -- )
   DESCRIPTOR-FAIL !  CURSOR-ABOVE !
   14 10 DESCRIPTOR-FAIL LABEL@ DESCRIPTOR
   12 DESCRIPTOR-FAIL LABEL@ ALIGNED
   12 14 CMP,  C-CC DESCRIPTOR-FAIL LABEL@ BCOND,
   12 12 14 SUB,
   12 10 CMP,  C-HI DESCRIPTOR-FAIL LABEL@ BCOND,
   10 10 12 SUB,
   10 CURSOR-ABOVE @ CMPI,  C-CC DESCRIPTOR-FAIL LABEL@ BCOND, ;

: EXIT-BOUNDS ( -- ) EMIT-FAIL ;

: EMIT-DATA ( -- label label )
   LBL CHECK-FAIL !  LBL CHECK-UNDER !  LBL CHECK-END !
   DATA-ENTRY LBL,
   SAVE-CHECK
   14 DATA STACK-ABI:BASE-CELL LDR,
   XDS CHECK-FAIL LABEL@ ALIGNED
   15 DATA STACK-ABI:CAP-CELL LDR,
   14 15 CHECK-FAIL LABEL@ DESCRIPTOR
   XDS 14 CMP,  C-CC CHECK-FAIL LABEL@ BCOND,
   14 XDS 14 SUB,
   14 15 CMP,  C-HI CHECK-FAIL LABEL@ BCOND,
   16 14 CMP,  C-HI CHECK-UNDER LABEL@ BCOND,
   15 15 14 SUB,
   17 15 CMP,  C-HI CHECK-FAIL LABEL@ BCOND,
   RETURN-CHECK
   CHECK-UNDER LABEL@ LBL,  EMIT-UNDERFLOW
   CHECK-FAIL LABEL@ LBL,  EMIT-FAIL
   CHECK-END LABEL@ LBL,
   DATA-ENTRY CHECK-END LABEL@ ;

: EMIT-FIXED ( n n label -- label label )
   CHECK-START !  FIXED-CAP !  FIXED-OFF !
   LBL CHECK-FAIL !  LBL CHECK-END !
   CHECK-START LABEL@ LBL,
   SAVE-CHECK
   14 DATA FIXED-OFF @ LDR,  15 FIXED-CAP @ LIT64,
   14 15 CMP,  C-HI CHECK-FAIL LABEL@ BCOND,
   16 14 CMP,  C-HI CHECK-FAIL LABEL@ BCOND,
   15 15 14 SUB,
   17 15 CMP,  C-HI CHECK-FAIL LABEL@ BCOND,
   RETURN-CHECK
   CHECK-FAIL LABEL@ LBL,  EMIT-FAIL
   CHECK-END LABEL@ LBL,
   CHECK-START LABEL@ CHECK-END LABEL@ ;

;package

\ data-stack ops (XDS points just past TOS; full-ascending); regs live in mnem.fs
: G-PUSH ( n -- )
   0 8 STACK-GUARD:CHECK-DATA
   XDS 0 STR,  XDS XDS $8 ADDI, ;

: G-POP ( n -- )
   8 0 STACK-GUARD:CHECK-DATA
   XDS XDS $8 SUBI,  XDS 0 LDR, ;
variable DOT-LBL  variable ATOI-LBL
variable RT-LPOS  variable RT-LLOOP  variable RT-LDONE

\ print x9 as signed decimal + newline (itoa into an sp buffer, then write(1,..)).
\ clobbers x9-x14 + 32 bytes of sp scratch; preserves XDS.
: G-PRINT9 ( -- )
   LBL RT-LPOS !  LBL RT-LLOOP !  LBL RT-LDONE !
   SP SP $20 SUBI,  12 SP $20 ADDI,
   13 $A MOVZ,  12 12 1 SUBI,  13 12 0 STRB,
   14 0 MOVZ,  9 0 CMPI,
   C-GE RT-LPOS LABEL@ BCOND,
   14 1 MOVZ,  9 SP 9 SUB,  RT-LPOS LABEL@ LBL,
   10 $A MOVZ,
   RT-LLOOP LABEL@ LBL,
   11 9 10 SDIV,  13 11 10 MUL,  13 9 13 SUB,
   13 13 $30 ADDI,  12 12 1 SUBI,  13 12 0 STRB,
   9 11 0 ADDI,  9 RT-LLOOP LABEL@ CBNZ,
   14 RT-LDONE LABEL@ CBZ,
   13 $2D MOVZ,  12 12 1 SUBI,  13 12 0 STRB,  RT-LDONE LABEL@ LBL,
   0 1 MOVZ,  1 12 0 ADDI,  2 SP $20 ADDI,  2 2 12 SUB,
   NR-WRITE SYS,
   SP SP $20 ADDI, ;

: EMIT-DOT ( -- )
   DOT-LBL LABEL@ LBL,  9 G-POP  G-PRINT9  RET, ;

\ Print x9 as UNSIGNED decimal + newline. Same itoa loop as G-PRINT9 but UDIV
\ and no sign handling. Clobbers x9-x13 + 32 bytes of sp scratch.
: G-PRINTU9 ( -- )
   SP SP $20 SUBI,  12 SP $20 ADDI,
   13 $A MOVZ,  12 12 1 SUBI,  13 12 0 STRB,
   10 $A MOVZ,
   LBL RT-LLOOP !  RT-LLOOP LABEL@ LBL,
   11 9 10 UDIV,  13 11 10 MUL,  13 9 13 SUB,
   13 13 $30 ADDI,  12 12 1 SUBI,  13 12 0 STRB,
   9 11 0 ADDI,  9 RT-LLOOP LABEL@ CBNZ,
   0 1 MOVZ,  1 12 0 ADDI,  2 SP $20 ADDI,  2 2 12 SUB,
   NR-WRITE SYS,
   SP SP $20 ADDI, ;

\ Write the single byte in x13 to stdout (emit/cr/space share it).
: G-EMITC ( -- )
   SP SP $10 SUBI,  13 SP 0 STRB,
   0 1 MOVZ,  1 SP 0 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   SP SP $10 ADDI, ;

\ ATOI: NUL-terminated decimal string at x9 -> push i64 (leading '-' ok). Leaf.
: EMIT-ATOI ( -- )
   ATOI-LBL LABEL@ LBL,
   LBL RT-LPOS !  LBL RT-LLOOP !  LBL RT-LDONE !
   10 0 MOVZ,
   11 1 MOVZ,
   12 9 0 LDRB,  12 $2D CMPI,
   C-NE RT-LPOS LABEL@ BCOND,
   11 0 MOVN,  9 9 1 ADDI,
   RT-LPOS LABEL@ LBL,
   RT-LLOOP LABEL@ LBL,
   12 9 0 LDRB,
   12 $30 CMPI,  C-LT RT-LDONE LABEL@ BCOND,
   12 $39 CMPI,  C-GT RT-LDONE LABEL@ BCOND,
   12 12 $30 SUBI,
   13 $A MOVZ,  10 10 13 MUL,  10 10 12 ADD,
   9 9 1 ADDI,  RT-LLOOP LABEL@ B,
   RT-LDONE LABEL@ LBL,
   10 10 11 MUL,
   10 G-PUSH
   RET, ;

;using
