\ asm-checked-test.f - checked ARM64 encoder layout regression.
\ Run after native refresh:
\ bin/hb --load lib/errors.f lib/string.f lib/test.f bootstrap/cg/asm-checked.fs tools/asm-checked-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require bootstrap/cg/asm-checked.fs

: R1 ( -- reg ) 1 >REG ;
: R2 ( -- reg ) 2 >REG ;
: R3 ( -- reg ) 3 >REG ;
: R7 ( -- reg ) 7 >REG ;
: O24 ( -- off ) 24 >OFF ;

TRUSTED: ACT-CHECK-REJECTS ( ptr u8 n -- )
   DIAGXT @ >r
   0 DIAGXT !
   CHECK! 0 T=
   r> DIAGXT ! ;

: ACT-TEST-LAYOUTS ( -- )
   R1 R2 R3 REG>N 0 A-RRR16 $00030041 T=
   R1 R2 3 0 A-RRI10 $00000C41 T=
   R1 2 3 0 A-MOVW $00600041 T=
   R1 R2 O24 0 A-LS-UOFF $00000C41 T=
   R7 0 A-R1-5 $000000E0 T=
   R2 4 0 A-CSET-LAYOUT $00005002 T= ;

: ACT-TEST-RRR ( -- )
   R1 R2 R3 A-ADD $8B030041 T=
   R1 R2 R3 A-SUB $CB030041 T=
   R1 R2 R3 A-MUL $9B037C41 T=
   R1 R2 R3 A-AND $8A030041 T=
   R1 R2 R3 A-ORR $AA030041 T=
   R1 R2 R3 A-EOR $CA030041 T= ;

: ACT-TEST-IMMEDIATE ( -- )
   R1 R2 3 A-ADDI $91000C41 T=
   R1 R2 3 A-SUBI $D1000C41 T=
   R1 R2 3 A-ANDI $92000C41 T=
   R1 R2 3 A-ORRI $B2000C41 T=
   R1 R2 3 A-EORI $D2000C41 T=
   R1 R2 3 A-ASRI $9343FC41 T= ;

: ACT-TEST-MOVE-WIDE ( -- )
   R1 2 3 A-MOVZ $D2E00041 T=
   R1 2 3 A-MOVK $F2E00041 T= ;

: ACT-TEST-LOAD-STORE ( -- )
   R1 R2 O24 A-LDR $F9400C41 T=
   R1 R2 O24 A-STR $F9000C41 T= ;

: ACT-TEST-DP2 ( -- )
   R1 R2 R3 A-SDIV $9AC30C41 T=
   R1 R2 R3 A-UDIV $9AC30841 T=
   R1 R2 R3 A-LSLV $9AC32041 T=
   R1 R2 R3 A-LSRV $9AC32441 T=
   R1 R2 R3 A-ASRV $9AC32841 T= ;

: ACT-TEST-SYSTEM ( -- )
   R7 A-BLR $D63F00E0 T=
   R7 A-BR $D61F00E0 T=
   R7 A-ICIVAU $D50B7527 T=
   R7 A-DCCVAU $D50B7B27 T=
   R2 4 A-CSET $9A9F57E2 T= ;

: ACT-TEST-REJECTS ( -- )
   s" BAD-ADD ( reg reg off -- n ) A-ADD" ACT-CHECK-REJECTS
   s" BAD-LDR ( reg reg reg -- n ) A-LDR" ACT-CHECK-REJECTS ;

: ACT-MAIN ( -- )
   T-RESET
   ACT-TEST-LAYOUTS
   ACT-TEST-RRR
   ACT-TEST-IMMEDIATE
   ACT-TEST-MOVE-WIDE
   ACT-TEST-LOAD-STORE
   ACT-TEST-DP2
   ACT-TEST-SYSTEM
   ACT-TEST-REJECTS
   T-REPORT
   s" asm-checked-test: ok" type cr ;

ACT-MAIN
