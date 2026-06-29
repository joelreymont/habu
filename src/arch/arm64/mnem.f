\ mnem.fs — icode-style mnemonic layer over the selfhost encoders: `19 19 8 ADDI,`
\ reads like src/cg source, so the engine-builder port stays a near-transcription.
\ Needs asm.fs + icode.fs (EMITW). One concern: mnemonic -> encode+emit.
9 constant T0   10 constant T1   11 constant T2
19 constant XDS  31 constant SP
 0 constant C-EQ   1 constant C-NE   2 constant C-CS   3 constant C-CC
 4 constant C-MI   5 constant C-PL   6 constant C-VS   7 constant C-VC
 8 constant C-HI   9 constant C-LS  10 constant C-GE  11 constant C-LT
12 constant C-GT  13 constant C-LE  14 constant C-AL

: MOVZ, ( n n -- )  0 MOVZHW EMITW ;

: MOVN, ( n n -- )  0 MOVNHW EMITW ;

: MOVK, ( n n n -- )  $10 / MOVKHW EMITW ;

: ADD, ( n n n -- )  ENC-ADD EMITW ;

: ADDI, ( n n n -- )  ENC-ADDI EMITW ;

: SUB, ( n n n -- )  ENC-SUB EMITW ;

: SUBI, ( n n n -- )  ENC-SUBI EMITW ;

: MUL, ( n n n -- )  ENC-MUL EMITW ;

: SDIV, ( n n n -- )  ENC-SDIV EMITW ;

: UDIV, ( n n n -- )  ENC-UDIV EMITW ;

: AND, ( n n n -- )  ENC-AND EMITW ;

: ORR, ( n n n -- )  ENC-ORR EMITW ;

: EOR, ( n n n -- )  ENC-EOR EMITW ;

: ANDI, ( n n n -- )  >LIMM ENC-ANDI EMITW ;

: ORRI, ( n n n -- )  >LIMM ENC-ORRI EMITW ;

: EORI, ( n n n -- )  >LIMM ENC-EORI EMITW ;

: LSLI, ( n n n -- )  ENC-LSLI EMITW ;

: LSRI, ( n n n -- )  ENC-LSRI EMITW ;

: ASRI, ( n n n -- )  ENC-ASRI EMITW ;

: LSLV, ( n n n -- )  ENC-LSLV EMITW ;

: LSRV, ( n n n -- )  ENC-LSRV EMITW ;

: CMP, ( n n -- )  ENC-CMP EMITW ;

: CMPI, ( n n -- )  ENC-CMPI EMITW ;

: CSET, ( n n -- )  ENC-CSET EMITW ;

: LDR, ( n n n -- )  ENC-LDR EMITW ;

: STR, ( n n n -- )  ENC-STR EMITW ;

: LDRB, ( n n n -- )  ENC-LDRB EMITW ;

: STRB, ( n n n -- )  ENC-STRB EMITW ;

: LDRW, ( n n n -- )  ENC-LDRW EMITW ;

: STRW, ( n n n -- )  ENC-STRW EMITW ;

: SVC, ( n -- )  ENC-SVC EMITW ;

: RET, ( -- ) ENC-RET EMITW ;

: BLR, ( n -- )  ENC-BLR EMITW ;

: BR, ( n -- )  ENC-BR EMITW ;

: BRK, ( -- ) ENC-BRK EMITW ;

: NOP, ( -- ) ENC-NOP EMITW ;

: ICIVAU, ( n -- )  ENC-ICIVAU EMITW ;

: DCCVAU, ( n -- )  ENC-DCCVAU EMITW ;

: DSB-ISH, ( -- ) ENC-DSB-ISH EMITW ;

: ISB, ( -- ) ENC-ISB EMITW ;

: FMOVXD, ( n n -- )  ENC-FMOVXD EMITW ;

: FMOVDX, ( n n -- )  ENC-FMOVDX EMITW ;

: FMOVDD, ( n n -- )  ENC-FMOVDD EMITW ;

: FADD, ( n n n -- )  ENC-FADD EMITW ;

: FSUB, ( n n n -- )  ENC-FSUB EMITW ;

: FMUL, ( n n n -- )  ENC-FMUL EMITW ;

: FDIV, ( n n n -- )  ENC-FDIV EMITW ;

: FNEG, ( n n -- )  ENC-FNEG EMITW ;

: FABS, ( n n -- )  ENC-FABS EMITW ;

: FSQRT, ( n n -- )  ENC-FSQRT EMITW ;

: FCMP, ( n n -- )  ENC-FCMP EMITW ;

: FCMP0, ( n -- )  ENC-FCMP0 EMITW ;

: SCVTF, ( n n -- )  ENC-SCVTF EMITW ;

: FCVTZS, ( n n -- )  ENC-FCVTZS EMITW ;
