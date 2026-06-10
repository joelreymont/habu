\ rt.fs — native runtime routines emitted into a program when used. Currently:
\ `.` (print the TOS as signed decimal + newline) via an itoa loop + a write
\ syscall, built directly in ICode. link.fs assigns DOT-LBL and emits EMIT-DOT
\ once per program that uses `.`. Leaf routine (only svc); preserves Xds.

require asm.fs

variable DOT-LBL
variable USES-DOT

\ registers: x9=n, x10=10, x11=q, x12=ptr, x13=digit, x14=neg ; buffer on sp
: EMIT-DOT ( -- )
   DOT-LBL @ LBL,
   XDS XDS 8 SUBI,  9 XDS 0 LDR,          \ x9 = pop()
   SP SP 32 SUBI,                          \ 32-byte scratch buffer at sp
   12 SP 32 ADDI,                          \ x12 = sp+32 (one past end)
   13 10 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,   \ *--x12 = '\n'
   14 0 MOVZ,                              \ neg = 0
   9 0 CMPI,
   NEWLBL {: lpos :}  C-GE lpos BCOND,     \ if n>=0 skip negate
   14 1 MOVZ,  9 SP 9 SUB,                 \ neg=1 ; n = -n
   lpos LBL,
   10 10 MOVZ,                             \ x10 = 10
   NEWLBL {: lloop :}  lloop LBL,
   11 9 10 SDIV,                           \ q = n/10
   13 11 10 MUL,  13 9 13 SUB,             \ r = n - q*10
   13 13 48 ADDI,                          \ r += '0'
   12 12 1 SUBI,  13 12 0 STRB,            \ *--ptr = digit
   9 11 0 ADDI,                            \ n = q
   9 lloop CBNZ,                           \ while n != 0
   NEWLBL {: lnosign :}  14 lnosign CBZ,
   13 45 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,   \ *--ptr = '-'
   lnosign LBL,
   0 1 MOVZ,  1 12 0 ADDI,                 \ x0=1(stdout), x1=ptr
   2 SP 32 ADDI,  2 2 12 SUB,              \ x2 = (sp+32) - ptr  (length incl '\n')
   16 4 MOVZ,  $80 SVC,                    \ write(1, ptr, len)
   SP SP 32 ADDI,                          \ restore sp
   RET, ;
