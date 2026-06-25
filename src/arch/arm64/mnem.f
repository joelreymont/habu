\ mnem.fs — icode-style mnemonic layer over the selfhost encoders: `19 19 8 ADDI,`
\ reads like src/cg source, so the engine-builder port stays a near-transcription.
\ Needs asm.fs + icode.fs (EMITW). One concern: mnemonic -> encode+emit.
9 constant T0   10 constant T1   11 constant T2
19 constant XDS  31 constant SP
 0 constant C-EQ   1 constant C-NE   2 constant C-CS   3 constant C-CC
 4 constant C-MI   5 constant C-PL   6 constant C-VS   7 constant C-VC
 8 constant C-HI   9 constant C-LS  10 constant C-GE  11 constant C-LT
12 constant C-GT  13 constant C-LE  14 constant C-AL

: MOVZ, ( n n -- ) {: RD imm :}  RD imm 0 MOVZHW EMITW ;

: MOVN, ( n n -- ) {: RD imm :}  RD imm 0 MOVNHW EMITW ;

: MOVK, ( n n n -- ) {: RD imm sh :}  RD imm sh 16 / MOVKHW EMITW ;

: ADD, ( n n n -- ) {: RD RN RM :}  RD RN RM ENC-ADD EMITW ;

: ADDI, ( n n n -- ) {: RD RN imm :}  RD RN imm ENC-ADDI EMITW ;

: SUB, ( n n n -- ) {: RD RN RM :}  RD RN RM ENC-SUB EMITW ;

: SUBI, ( n n n -- ) {: RD RN imm :}  RD RN imm ENC-SUBI EMITW ;

: MUL, ( n n n -- ) {: RD RN RM :}  RD RN RM ENC-MUL EMITW ;

: SDIV, ( n n n -- ) {: RD RN RM :}  RD RN RM ENC-SDIV EMITW ;

: UDIV, ( n n n -- ) {: RD RN RM :}  RD RN RM ENC-UDIV EMITW ;

: AND, ( n n n -- ) {: RD RN RM :}  RD RN RM ENC-AND EMITW ;

: ORR, ( n n n -- ) {: RD RN RM :}  RD RN RM ENC-ORR EMITW ;

: EOR, ( n n n -- ) {: RD RN RM :}  RD RN RM ENC-EOR EMITW ;

: ANDI, ( n n n -- ) {: RD RN mask :}  RD RN mask >LIMM ENC-ANDI EMITW ;

: ORRI, ( n n n -- ) {: RD RN mask :}  RD RN mask >LIMM ENC-ORRI EMITW ;

: EORI, ( n n n -- ) {: RD RN mask :}  RD RN mask >LIMM ENC-EORI EMITW ;

: LSLI, ( n n n -- ) {: RD RN sh :}  RD RN sh ENC-LSLI EMITW ;

: LSRI, ( n n n -- ) {: RD RN sh :}  RD RN sh ENC-LSRI EMITW ;

: ASRI, ( n n n -- ) {: RD RN sh :}  RD RN sh ENC-ASRI EMITW ;

: LSLV, ( n n n -- ) {: RD RN RM :}  RD RN RM ENC-LSLV EMITW ;

: LSRV, ( n n n -- ) {: RD RN RM :}  RD RN RM ENC-LSRV EMITW ;

: CMP, ( n n -- ) {: RN RM :}  RN RM ENC-CMP EMITW ;

: CMPI, ( n n -- ) {: RN imm :}  RN imm ENC-CMPI EMITW ;

: CSET, ( n n -- ) {: RD cond :}  RD cond ENC-CSET EMITW ;

: LDR, ( n n n -- ) {: rt RN off :}  rt RN off ENC-LDR EMITW ;

: STR, ( n n n -- ) {: rt RN off :}  rt RN off ENC-STR EMITW ;

: LDRB, ( n n n -- ) {: rt RN off :}  rt RN off ENC-LDRB EMITW ;

: STRB, ( n n n -- ) {: rt RN off :}  rt RN off ENC-STRB EMITW ;

: LDRW, ( n n n -- ) {: rt RN off :}  rt RN off ENC-LDRW EMITW ;

: STRW, ( n n n -- ) {: rt RN off :}  rt RN off ENC-STRW EMITW ;

: SVC, ( n -- ) {: imm :}  imm ENC-SVC EMITW ;

: RET, ( -- ) ENC-RET EMITW ;

: BLR, ( n -- ) {: RN :}  RN ENC-BLR EMITW ;

: BR, ( n -- ) {: RN :}  RN ENC-BR EMITW ;

: BRK, ( -- ) ENC-BRK EMITW ;

: NOP, ( -- ) ENC-NOP EMITW ;

: ICIVAU, ( n -- ) {: rt :}  rt ENC-ICIVAU EMITW ;

: DCCVAU, ( n -- ) {: rt :}  rt ENC-DCCVAU EMITW ;

: DSB-ISH, ( -- ) ENC-DSB-ISH EMITW ;

: ISB, ( -- ) ENC-ISB EMITW ;

: FMOVXD, ( n n -- ) {: d n :}  d n ENC-FMOVXD EMITW ;

: FMOVDX, ( n n -- ) {: d n :}  d n ENC-FMOVDX EMITW ;

: FMOVDD, ( n n -- ) {: d n :}  d n ENC-FMOVDD EMITW ;

: FADD, ( n n n -- ) {: d n m :}  d n m ENC-FADD EMITW ;

: FSUB, ( n n n -- ) {: d n m :}  d n m ENC-FSUB EMITW ;

: FMUL, ( n n n -- ) {: d n m :}  d n m ENC-FMUL EMITW ;

: FDIV, ( n n n -- ) {: d n m :}  d n m ENC-FDIV EMITW ;

: FNEG, ( n n -- ) {: d n :}  d n ENC-FNEG EMITW ;

: FABS, ( n n -- ) {: d n :}  d n ENC-FABS EMITW ;

: FSQRT, ( n n -- ) {: d n :}  d n ENC-FSQRT EMITW ;

: FCMP, ( n n -- ) {: n m :}  n m ENC-FCMP EMITW ;

: FCMP0, ( n -- ) {: n :}  n ENC-FCMP0 EMITW ;

: SCVTF, ( n n -- ) {: d n :}  d n ENC-SCVTF EMITW ;

: FCVTZS, ( n n -- ) {: d n :}  d n ENC-FCVTZS EMITW ;
