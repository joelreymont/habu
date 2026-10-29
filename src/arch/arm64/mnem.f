\ mnem.f - one word per mnemonic: encode with asm.f, emit with icode.f EMITW.
\ The encoders are imported and called bare: a qualifier on every one-line
\ mnemonic would be the whole file. Condition-code names live in asm.f beside
\ ?COND.
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
using A64ASM
using A64ICODE

9 constant T0   10 constant T1   11 constant T2
19 constant XDS  31 constant SP

: MOVZ, ( n n -- )  0 MOVZHW EMITW ;

: MOVN, ( n n -- )  0 MOVNHW EMITW ;

\ The shift is in bits; a shift that is not a whole half is refused, not rounded.
: MOVK, ( n n n -- )  $10 SCALE/ MOVKHW EMITW ;

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

: CSETM, ( n n -- )  ENC-CSETM EMITW ;

: CSEL, ( n n n n -- )  ENC-CSEL EMITW ;

: LDR, ( n n n -- )  ENC-LDR EMITW ;

: STR, ( n n n -- )  ENC-STR EMITW ;

: LDRB, ( n n n -- )  ENC-LDRB EMITW ;

: STRB, ( n n n -- )  ENC-STRB EMITW ;

: LDRW, ( n n n -- )  ENC-LDRW EMITW ;

: STRW, ( n n n -- )  ENC-STRW EMITW ;

\ The data-stack pair: push is STRPOST, +8; pop is LDRPRE, -8. The offset is the
\ distance the base register travels.
: STRPOST, ( n n n -- )  ENC-STRPOST EMITW ;

: LDRPRE, ( n n n -- )  ENC-LDRPRE EMITW ;

: LDAR, ( n n -- )  ENC-LDAR EMITW ;

: STLR, ( n n -- )  ENC-STLR EMITW ;

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

: FMOVDI, ( n n -- )  ENC-FMOVDI EMITW ;

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

: FCSEL, ( n n n n -- )  ENC-FCSEL EMITW ;

: SCVTF, ( n n -- )  ENC-SCVTF EMITW ;

: FCVTZS, ( n n -- )  ENC-FCVTZS EMITW ;

;using
;using
