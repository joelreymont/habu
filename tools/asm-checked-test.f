\ asm-checked-test.f - checked ARM64 encoder layout regression.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f bootstrap/cg/asm-checked.fs tools/asm-checked-test.f

: ACT-TEST-LAYOUTS ( -- )
   1 2 3 0 A-RRR16 $00030041 T=
   1 2 3 0 A-RRI10 $00000C41 T=
   1 2 3 0 A-MOVW $00600041 T=
   1 2 24 0 A-LS-UOFF $00000C41 T=
   7 0 A-R1-5 $000000E0 T=
   2 4 0 A-CSET-LAYOUT $00005002 T= ;

: ACT-TEST-RRR ( -- )
   1 2 3 A-ADD $8B030041 T=
   1 2 3 A-SUB $CB030041 T=
   1 2 3 A-MUL $9B037C41 T=
   1 2 3 A-AND $8A030041 T=
   1 2 3 A-ORR $AA030041 T=
   1 2 3 A-EOR $CA030041 T= ;

: ACT-TEST-IMMEDIATE ( -- )
   1 2 3 A-ADDI $91000C41 T=
   1 2 3 A-SUBI $D1000C41 T=
   1 2 3 A-ANDI $92000C41 T=
   1 2 3 A-ORRI $B2000C41 T=
   1 2 3 A-EORI $D2000C41 T=
   1 2 3 A-ASRI $9343FC41 T= ;

: ACT-TEST-MOVE-WIDE ( -- )
   1 2 3 A-MOVZ $D2E00041 T=
   1 2 3 A-MOVK $F2E00041 T= ;

: ACT-TEST-LOAD-STORE ( -- )
   1 2 24 A-LDR $F9400C41 T=
   1 2 24 A-STR $F9000C41 T= ;

: ACT-TEST-DP2 ( -- )
   1 2 3 A-SDIV $9AC30C41 T=
   1 2 3 A-UDIV $9AC30841 T=
   1 2 3 A-LSLV $9AC32041 T=
   1 2 3 A-LSRV $9AC32441 T=
   1 2 3 A-ASRV $9AC32841 T= ;

: ACT-TEST-SYSTEM ( -- )
   7 A-BLR $D63F00E0 T=
   7 A-BR $D61F00E0 T=
   7 A-ICIVAU $D50B7527 T=
   7 A-DCCVAU $D50B7B27 T=
   2 4 A-CSET $9A9F57E2 T= ;

: ACT-MAIN ( -- )
   T-RESET
   ACT-TEST-LAYOUTS
   ACT-TEST-RRR
   ACT-TEST-IMMEDIATE
   ACT-TEST-MOVE-WIDE
   ACT-TEST-LOAD-STORE
   ACT-TEST-DP2
   ACT-TEST-SYSTEM
   T-REPORT
   s" asm-checked-test: ok" type cr ;

ACT-MAIN
