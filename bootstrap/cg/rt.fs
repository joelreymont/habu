\ rt.fs — integer and byte output emitted by the recovery engine's primitives.

require asm.fs
require sys.fs

\ Print x9 as signed decimal + newline (itoa into an sp buffer, then write(1,…)).
\ Clobbers x9-x14 + 32 bytes of sp scratch; preserves Xds.  registers:
\ x9=n, x10=10, x11=q, x12=ptr, x13=digit, x14=neg.
\ The digit loop divides UNSIGNED: MIN-N is the one cell whose negation
\ overflows, and UDIV reads the pattern left behind as the magnitude it is
\ (2^63). MIRROR of src/habu/rt.f G-PRINT9.
: G-PRINT9 ( -- )
   SP SP 32 SUBI,  12 SP 32 ADDI,              \ x12 = end of a 32-byte sp buffer
   13 10 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,   \ *--ptr = '\n'
   14 0 MOVZ,  9 0 CMPI,
   LBL {: lpos :}  C-GE lpos BCOND,         \ n>=0 ? skip negate
   14 1 MOVZ,  9 SP 9 SUB,  lpos LBL,          \ neg=1 ; n = -n
   10 10 MOVZ,
   LBL {: lloop :}  lloop LBL,
   11 9 10 UDIV,  13 11 10 MUL,  13 9 13 SUB,  \ q=n/10 ; r=n-q*10
   13 13 48 ADDI,  12 12 1 SUBI,  13 12 0 STRB,
   9 11 0 ADDI,  9 lloop CBNZ,                 \ n=q ; while n!=0
   LBL {: lns :}  14 lns CBZ,
   13 45 MOVZ,  12 12 1 SUBI,  13 12 0 STRB,  lns LBL,   \ prepend '-'
   0 1 MOVZ,  1 12 0 ADDI,  2 SP 32 ADDI,  2 2 12 SUB,
   NR-WRITE SYS,                        \ write(1, ptr, len)
   SP SP 32 ADDI, ;

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
