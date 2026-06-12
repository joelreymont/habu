\ mnem.fs — icode-style mnemonic layer over the selfhost encoders: `19 19 8 ADDI,`
\ reads like src/cg source, so the engine-builder port stays a near-transcription.
\ Needs asm.fs + icode.fs (EMITW). One concern: mnemonic -> encode+emit.
9 constant T0   10 constant T1   11 constant T2
19 constant XDS  31 constant SP
 0 constant C-EQ   1 constant C-NE   2 constant C-CS   3 constant C-CC
 4 constant C-MI   5 constant C-PL   6 constant C-VS   7 constant C-VC
 8 constant C-HI   9 constant C-LS  10 constant C-GE  11 constant C-LT
12 constant C-GT  13 constant C-LE  14 constant C-AL

: MOVZ, {: RD imm :}  RD imm 0 MOVZHW EMITW ;

: MOVN, {: RD imm :}  RD imm 0 MOVNHW EMITW ;

: MOVK, {: RD imm sh :}  RD imm sh 16 / MOVKHW EMITW ;

: ADD,  {: RD RN RM :}  RD RN RM ENC-ADD EMITW ;

: ADDI, {: RD RN imm :}  RD RN imm ENC-ADDI EMITW ;

: SUB,  {: RD RN RM :}  RD RN RM ENC-SUB EMITW ;

: SUBI, {: RD RN imm :}  RD RN imm ENC-SUBI EMITW ;

: MUL,  {: RD RN RM :}  RD RN RM ENC-MUL EMITW ;

: SDIV, {: RD RN RM :}  RD RN RM ENC-SDIV EMITW ;

: UDIV, {: RD RN RM :}  RD RN RM ENC-UDIV EMITW ;

: AND,  {: RD RN RM :}  RD RN RM ENC-AND EMITW ;

: ORR,  {: RD RN RM :}  RD RN RM ENC-ORR EMITW ;

: EOR,  {: RD RN RM :}  RD RN RM ENC-EOR EMITW ;

: ANDI, {: RD RN mask :}  RD RN mask >LIMM ENC-ANDI EMITW ;

: ORRI, {: RD RN mask :}  RD RN mask >LIMM ENC-ORRI EMITW ;

: EORI, {: RD RN mask :}  RD RN mask >LIMM ENC-EORI EMITW ;

: LSLI, {: RD RN sh :}  RD RN sh ENC-LSLI EMITW ;

: LSRI, {: RD RN sh :}  RD RN sh ENC-LSRI EMITW ;

: ASRI, {: RD RN sh :}  RD RN sh ENC-ASRI EMITW ;

: LSLV, {: RD RN RM :}  RD RN RM ENC-LSLV EMITW ;

: LSRV, {: RD RN RM :}  RD RN RM ENC-LSRV EMITW ;

: CMP,  {: RN RM :}  RN RM ENC-CMP EMITW ;

: CMPI, {: RN imm :}  RN imm ENC-CMPI EMITW ;

: CSET, {: RD cond :}  RD cond ENC-CSET EMITW ;

: LDR,  {: rt RN off :}  rt RN off ENC-LDR EMITW ;

: STR,  {: rt RN off :}  rt RN off ENC-STR EMITW ;

: LDRB, {: rt RN off :}  rt RN off ENC-LDRB EMITW ;

: STRB, {: rt RN off :}  rt RN off ENC-STRB EMITW ;

: LDRW, {: rt RN off :}  rt RN off ENC-LDRW EMITW ;

: STRW, {: rt RN off :}  rt RN off ENC-STRW EMITW ;

: SVC,  {: imm :}  imm ENC-SVC EMITW ;

: RET,  ENC-RET EMITW ;

: BLR,  {: RN :}  RN ENC-BLR EMITW ;

: BR,   {: RN :}  RN ENC-BR EMITW ;

: BRK,  ENC-BRK EMITW ;

: NOP,  ENC-NOP EMITW ;

: ICIVAU, {: rt :}  rt ENC-ICIVAU EMITW ;

: DCCVAU, {: rt :}  rt ENC-DCCVAU EMITW ;

: DSB-ISH,  ENC-DSB-ISH EMITW ;

: ISB,      ENC-ISB EMITW ;

: FMOVXD, {: d n :}  d n ENC-FMOVXD EMITW ;

: FMOVDX, {: d n :}  d n ENC-FMOVDX EMITW ;

: FMOVDD, {: d n :}  d n ENC-FMOVDD EMITW ;

: FADD,  {: d n m :}  d n m ENC-FADD EMITW ;

: FSUB,  {: d n m :}  d n m ENC-FSUB EMITW ;

: FMUL,  {: d n m :}  d n m ENC-FMUL EMITW ;

: FDIV,  {: d n m :}  d n m ENC-FDIV EMITW ;

: FNEG,  {: d n :}  d n ENC-FNEG EMITW ;

: FABS,  {: d n :}  d n ENC-FABS EMITW ;

: FSQRT, {: d n :}  d n ENC-FSQRT EMITW ;

: FCMP,  {: n m :}  n m ENC-FCMP EMITW ;

: FCMP0, {: n :}  n ENC-FCMP0 EMITW ;

: SCVTF, {: d n :}  d n ENC-SCVTF EMITW ;

: FCVTZS, {: d n :}  d n ENC-FCVTZS EMITW ;
