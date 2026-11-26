\ mnem.f - one word per mnemonic: encode with asm.f, emit with icode.f EMITW.
\ Condition-code names live in asm.f beside ?COND.
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f

package A64ICODE
public
9 constant T0   10 constant T1   11 constant T2
ENGINE-GPR:A64-DSTACK constant XDS  31 constant SP

: MOVZ, ( n n -- )  0 A64ASM:MOVZHW EMITW ;

: MOVN, ( n n -- )  0 A64ASM:MOVNHW EMITW ;

\ The shift is in bits; a shift that is not a whole half is rejected, not rounded.
: MOVK, ( n n n -- )  $10 A64ASM:SCALE/ A64ASM:MOVKHW EMITW ;

: ADD, ( n n n -- )  A64ASM:ENC-ADD EMITW ;

: ADDI, ( n n n -- )  A64ASM:ENC-ADDI EMITW ;

: SUB, ( n n n -- )  A64ASM:ENC-SUB EMITW ;

: SUBI, ( n n n -- )  A64ASM:ENC-SUBI EMITW ;

: MUL, ( n n n -- )  A64ASM:ENC-MUL EMITW ;

: SDIV, ( n n n -- )  A64ASM:ENC-SDIV EMITW ;

: UDIV, ( n n n -- )  A64ASM:ENC-UDIV EMITW ;

: AND, ( n n n -- )  A64ASM:ENC-AND EMITW ;

: ORR, ( n n n -- )  A64ASM:ENC-ORR EMITW ;

: EOR, ( n n n -- )  A64ASM:ENC-EOR EMITW ;

: ANDI, ( n n n -- )  A64ASM:>LIMM A64ASM:ENC-ANDI EMITW ;

: ORRI, ( n n n -- )  A64ASM:>LIMM A64ASM:ENC-ORRI EMITW ;

: EORI, ( n n n -- )  A64ASM:>LIMM A64ASM:ENC-EORI EMITW ;

: LSLI, ( n n n -- )  A64ASM:ENC-LSLI EMITW ;

: LSRI, ( n n n -- )  A64ASM:ENC-LSRI EMITW ;

: ASRI, ( n n n -- )  A64ASM:ENC-ASRI EMITW ;

: LSLV, ( n n n -- )  A64ASM:ENC-LSLV EMITW ;

: LSRV, ( n n n -- )  A64ASM:ENC-LSRV EMITW ;

: CMP, ( n n -- )  A64ASM:ENC-CMP EMITW ;

: CMPI, ( n n -- )  A64ASM:ENC-CMPI EMITW ;

: CSET, ( n n -- )  A64ASM:ENC-CSET EMITW ;

: CSETM, ( n n -- )  A64ASM:ENC-CSETM EMITW ;

: CSEL, ( n n n n -- )  A64ASM:ENC-CSEL EMITW ;

: LDR, ( n n n -- )  A64ASM:ENC-LDR EMITW ;

: STR, ( n n n -- )  A64ASM:ENC-STR EMITW ;

: LDRB, ( n n n -- )  A64ASM:ENC-LDRB EMITW ;

: STRB, ( n n n -- )  A64ASM:ENC-STRB EMITW ;

: LDRH, ( n n n -- )  A64ASM:ENC-LDRH EMITW ;

: STRH, ( n n n -- )  A64ASM:ENC-STRH EMITW ;

: LDRW, ( n n n -- )  A64ASM:ENC-LDRW EMITW ;

: STRW, ( n n n -- )  A64ASM:ENC-STRW EMITW ;

\ The data-stack pair: push is STRPOST, +8; pop is LDRPRE, -8. The offset is the
\ distance the base register travels.
: STRPOST, ( n n n -- )  A64ASM:ENC-STRPOST EMITW ;

: LDRPRE, ( n n n -- )  A64ASM:ENC-LDRPRE EMITW ;

: LDAR, ( n n -- )  A64ASM:ENC-LDAR EMITW ;

: STLR, ( n n -- )  A64ASM:ENC-STLR EMITW ;

: LDARW, ( n n -- )  A64ASM:ENC-LDARW EMITW ;

: STLRW, ( n n -- )  A64ASM:ENC-STLRW EMITW ;

: SVC, ( n -- )  A64ASM:ENC-SVC EMITW ;

: RET, ( -- ) A64ASM:ENC-RET EMITW ;

: BLR, ( n -- )  A64ASM:ENC-BLR EMITW ;

: BR, ( n -- )  A64ASM:ENC-BR EMITW ;

: BRK, ( -- ) A64ASM:ENC-BRK EMITW ;

: NOP, ( -- ) A64ASM:ENC-NOP EMITW ;

: ICIVAU, ( n -- )  A64ASM:ENC-ICIVAU EMITW ;

: DCCVAU, ( n -- )  A64ASM:ENC-DCCVAU EMITW ;

: DSB-ISH, ( -- ) A64ASM:ENC-DSB-ISH EMITW ;

: DMB-ISH, ( -- ) A64ASM:ENC-DMB-ISH EMITW ;

: ISB, ( -- ) A64ASM:ENC-ISB EMITW ;

: YIELD, ( -- ) A64ASM:ENC-YIELD EMITW ;

: CASAL, ( n n n -- )  A64ASM:ENC-CASAL EMITW ;

: LDADDAL, ( n n n -- )  A64ASM:ENC-LDADDAL EMITW ;

: FMOVXD, ( n n -- )  A64ASM:ENC-FMOVXD EMITW ;

: FMOVDX, ( n n -- )  A64ASM:ENC-FMOVDX EMITW ;

: FADD, ( n n n -- )  A64ASM:ENC-FADD EMITW ;

: FSUB, ( n n n -- )  A64ASM:ENC-FSUB EMITW ;

: FMUL, ( n n n -- )  A64ASM:ENC-FMUL EMITW ;

: FDIV, ( n n n -- )  A64ASM:ENC-FDIV EMITW ;

: FNEG, ( n n -- )  A64ASM:ENC-FNEG EMITW ;

: FABS, ( n n -- )  A64ASM:ENC-FABS EMITW ;

: FSQRT, ( n n -- )  A64ASM:ENC-FSQRT EMITW ;

: FCMP, ( n n -- )  A64ASM:ENC-FCMP EMITW ;

: FCMP0, ( n -- )  A64ASM:ENC-FCMP0 EMITW ;

: SCVTF, ( n n -- )  A64ASM:ENC-SCVTF EMITW ;

: FCVTZS, ( n n -- )  A64ASM:ENC-FCVTZS EMITW ;

: LD1V-POST, ( n n -- )  A64ASM:ENC-LD1V-POST EMITW ;

: LD1V4-POST, ( n n -- )  A64ASM:ENC-LD1V4-POST EMITW ;

: LD2V, ( n n -- )  A64ASM:ENC-LD2V EMITW ;

: ST2V, ( n n -- )  A64ASM:ENC-ST2V EMITW ;

: REV32V, ( n n -- )  A64ASM:ENC-REV32V EMITW ;

: ADD4S, ( n n n -- )  A64ASM:ENC-ADD4S EMITW ;

: MOVV, ( n n -- )  A64ASM:ENC-MOVV EMITW ;

: SHA256H, ( n n n -- )  A64ASM:ENC-SHA256H EMITW ;

: SHA256H2, ( n n n -- )  A64ASM:ENC-SHA256H2 EMITW ;

: SHA256SU0, ( n n -- )  A64ASM:ENC-SHA256SU0 EMITW ;

: SHA256SU1, ( n n n -- )  A64ASM:ENC-SHA256SU1 EMITW ;
;package
