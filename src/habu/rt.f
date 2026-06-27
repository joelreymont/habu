\ rt.fs — native runtime routines for the ENGINE-BUILDER port. Emits stack and
\ printer instruction sequences via mnem.fs. Labels are allocated in ONE locals
\ group per word (the standalone mis-reads a second {: :} group).
\ data-stack ops (XDS points just past TOS; full-ascending); regs live in mnem.fs
: G-PUSH ( n -- )
   {: reg :}  reg XDS 0 STR,  XDS XDS 8 ADDI, ;

: G-POP ( n -- )
   {: reg :}  XDS XDS 8 SUBI,  reg XDS 0 LDR, ;
variable DOT-LBL  variable ATOI-LBL

\ print x9 as signed decimal + newline (itoa into an sp buffer, then write(1,..)).
\ clobbers x9-x14 + 32 bytes of sp scratch; preserves XDS.
: G-PRINT9 ( -- )
   LBL LBL LBL {: lpos lloop lns :}
   SP SP 32 SUBI,  12 SP 32 ADDI,
   13 10 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,
   14 0 MOVZ,  9 0 CMPI,
   C-GE lpos BCOND,
   14 1 MOVZ,  9 SP 9 SUB,  lpos LBL,
   10 10 MOVZ,
   lloop LBL,
   11 9 10 SDIV,  13 11 10 MUL,  13 9 13 SUB,
   13 13 48 ADDI,  12 12 1 SUBI,  13 12 0 STRB,
   9 11 0 ADDI,  9 lloop CBNZ,
   14 lns CBZ,
   13 45 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,  lns LBL,
   0 1 MOVZ,  1 12 0 ADDI,  2 SP 32 ADDI,  2 2 12 SUB,
   NR-WRITE SYS,
   SP SP 32 ADDI, ;

: EMIT-DOT ( -- )
   DOT-LBL LABEL@ LBL,  XDS XDS 8 SUBI,  9 XDS 0 LDR,  G-PRINT9  RET, ;

\ Print x9 as UNSIGNED decimal + newline. Same itoa loop as G-PRINT9 but UDIV
\ and no sign handling. Clobbers x9-x13 + 32 bytes of sp scratch.
: G-PRINTU9 ( -- )
   SP SP 32 SUBI,  12 SP 32 ADDI,
   13 10 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,
   10 10 MOVZ,
   LBL {: lloop :}  lloop LBL,
   11 9 10 UDIV,  13 11 10 MUL,  13 9 13 SUB,
   13 13 48 ADDI,  12 12 1 SUBI,  13 12 0 STRB,
   9 11 0 ADDI,  9 lloop CBNZ,
   0 1 MOVZ,  1 12 0 ADDI,  2 SP 32 ADDI,  2 2 12 SUB,
   NR-WRITE SYS,
   SP SP 32 ADDI, ;

\ Write the single byte in x13 to stdout (emit/cr/space share it).
: G-EMITC ( -- )
   SP SP 16 SUBI,  13 SP 0 STRB,
   0 1 MOVZ,  1 SP 0 ADDI,  2 1 MOVZ,  NR-WRITE SYS,
   SP SP 16 ADDI, ;

\ ATOI: NUL-terminated decimal string at x9 -> push i64 (leading '-' ok). Leaf.
: EMIT-ATOI ( -- )
   ATOI-LBL LABEL@ LBL,
   LBL LBL LBL {: lpos lloop ldone :}
   10 0 MOVZ,
   11 1 MOVZ,
   12 9 0 LDRB,  12 45 CMPI,
   C-NE lpos BCOND,
   11 0 MOVN,  9 9 1 ADDI,
   lpos LBL,
   lloop LBL,
   12 9 0 LDRB,
   12 48 CMPI,  C-LT ldone BCOND,
   12 57 CMPI,  C-GT ldone BCOND,
   12 12 48 SUBI,
   13 10 MOVZ,  10 10 13 MUL,  10 10 12 ADD,
   9 9 1 ADDI,  lloop B,
   ldone LBL,
   10 10 11 MUL,
   10 G-PUSH
   RET, ;
