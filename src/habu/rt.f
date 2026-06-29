\ rt.fs — native runtime routines for the ENGINE-BUILDER port. Emits stack and
\ printer instruction sequences via mnem.fs. Keep this stage-source file
\ local-free so the Gforth recovery compiler can check it.
\ data-stack ops (XDS points just past TOS; full-ascending); regs live in mnem.fs
: G-PUSH ( n -- )
   XDS 0 STR,  XDS XDS $8 ADDI, ;

: G-POP ( n -- )
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
   DOT-LBL LABEL@ LBL,  XDS XDS 8 SUBI,  9 XDS 0 LDR,  G-PRINT9  RET, ;

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
