\ rt.fs — native runtime routines for the ENGINE-BUILDER port, transcribed from
\ bootstrap/cg/rt.fs + the g-push/g-pop stack templates from bootstrap/cg/templ.fs. Emits the
\ same instruction sequences via mnem.fs (golden-tested word-for-word in
\ test/t-sh-rt.fs). Labels are allocated in ONE locals group per word (the
\ standalone mis-reads a second {: :} group).
\ data-stack ops (XDS points just past TOS; full-ascending); regs live in mnem.fs
: g-push {: reg :}  reg XDS 0 STR,  XDS XDS 8 ADDI, ;
: g-pop  {: reg :}  XDS XDS 8 SUBI,  reg XDS 0 LDR, ;
variable DOT-LBL  variable ATOI-LBL
\ print x9 as signed decimal + newline (itoa into an sp buffer, then write(1,..)).
\ clobbers x9-x14 + 32 bytes of sp scratch; preserves XDS.
: g-print9
   NEWLBL NEWLBL NEWLBL {: lpos lloop lns :}
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
   16 4 MOVZ,  $80 SVC,
   SP SP 32 ADDI, ;
: EMIT-DOT  DOT-LBL @ LBL,  XDS XDS 8 SUBI,  9 XDS 0 LDR,  g-print9  RET, ;
\ ATOI: NUL-terminated decimal string at x9 -> push i64 (leading '-' ok). Leaf.
: EMIT-ATOI
   ATOI-LBL @ LBL,
   NEWLBL NEWLBL NEWLBL {: lpos lloop ldone :}
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
   10 g-push
   RET, ;
