\ rt.fs — native runtime routines emitted into a program when used. Currently:
\ `.` (print the TOS as signed decimal + newline) via an itoa loop + a write
\ syscall, built directly in ICode. link.fs assigns DOT-LBL and emits EMIT-DOT
\ once per program that uses `.`. Leaf routine (only svc); preserves Xds.

require asm.fs
require sys.fs

variable DOT-LBL
variable USES-DOT
variable ATOI-LBL

\ Print x9 as signed decimal + newline (itoa into an sp buffer, then write(1,…)).
\ Shared by the AOT `.` routine (EMIT-DOT) and the native Forth `.` primitive.
\ Clobbers x9-x14 + 32 bytes of sp scratch; preserves Xds.  registers:
\ x9=n, x10=10, x11=q, x12=ptr, x13=digit, x14=neg.
: G-PRINT9 ( -- )
   SP SP 32 SUBI,  12 SP 32 ADDI,              \ x12 = end of a 32-byte sp buffer
   13 10 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,   \ *--ptr = '\n'
   14 0 MOVZ,  9 0 CMPI,
   NEWLBL {: lpos :}  C-GE lpos BCOND,         \ n>=0 ? skip negate
   14 1 MOVZ,  9 SP 9 SUB,  lpos LBL,          \ neg=1 ; n = -n
   10 10 MOVZ,
   NEWLBL {: lloop :}  lloop LBL,
   11 9 10 SDIV,  13 11 10 MUL,  13 9 13 SUB,  \ q=n/10 ; r=n-q*10
   13 13 48 ADDI,  12 12 1 SUBI,  13 12 0 STRB,
   9 11 0 ADDI,  9 lloop CBNZ,                 \ n=q ; while n!=0
   NEWLBL {: lns :}  14 lns CBZ,
   13 45 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,  lns LBL,   \ prepend '-'
   0 1 MOVZ,  1 12 0 ADDI,  2 SP 32 ADDI,  2 2 12 SUB,
   NR-WRITE SYS,                        \ write(1, ptr, len)
   SP SP 32 ADDI, ;

: EMIT-DOT ( -- )  DOT-LBL @ LBL,  XDS XDS 8 SUBI,  9 XDS 0 LDR,  G-PRINT9  RET, ;

\ Print x9 as UNSIGNED decimal + newline. Same itoa loop as G-PRINT9 but UDIV
\ and no sign handling. Clobbers x9-x13 + 32 bytes of sp scratch.
: G-PRINTU9 ( -- )
   SP SP 32 SUBI,  12 SP 32 ADDI,
   13 10 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,
   10 10 MOVZ,
   NEWLBL {: lloop :}  lloop LBL,
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

\ ATOI: parse a NUL-terminated decimal string at x9 -> push i64 on the data
\ stack. Handles a leading '-'. Leaf (pushes via Xds). Used by CLI entry.
: EMIT-ATOI ( -- )
   ATOI-LBL @ LBL,
   10 0 MOVZ,                              \ result = 0
   11 1 MOVZ,                              \ sign = 1
   12 9 0 LDRB,  12 45 CMPI,               \ first char == '-' ?
   NEWLBL {: lpos :}  C-NE lpos BCOND,
   11 0 MOVN,  9 9 1 ADDI,                 \ sign = -1 ; ptr++
   lpos LBL,
   NEWLBL {: lloop :}  NEWLBL {: ldone :}
   lloop LBL,
   12 9 0 LDRB,                            \ c = *ptr
   12 48 CMPI,  C-LT ldone BCOND,          \ c < '0' -> done
   12 57 CMPI,  C-GT ldone BCOND,          \ c > '9' -> done
   12 12 48 SUBI,                          \ c -= '0'
   13 10 MOVZ,  10 10 13 MUL,  10 10 12 ADD,   \ result = result*10 + c
   9 9 1 ADDI,  lloop B,                   \ ptr++
   ldone LBL,
   10 10 11 MUL,                           \ result *= sign
   10 G-PUSH
   RET, ;
