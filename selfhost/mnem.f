\ mnem.fs — icode-style mnemonic layer over the selfhost encoders: `19 19 8 ADDI,`
\ reads like src/cg source, so the engine-builder port stays a near-transcription.
\ Needs asm.fs + icode.fs (EMITW). One concern: mnemonic -> encode+emit.
9 constant T0   10 constant T1   11 constant T2
19 constant XDS  31 constant SP
 0 constant C-EQ   1 constant C-NE   2 constant C-CS   3 constant C-CC
 4 constant C-MI   5 constant C-PL   6 constant C-VS   7 constant C-VC
 8 constant C-HI   9 constant C-LS  10 constant C-GE  11 constant C-LT
12 constant C-GT  13 constant C-LE  14 constant C-AL
: MOVZ, {: rd imm :}  rd imm 0 MOVZHW EMITW ;
: MOVN, {: rd imm :}  rd imm 0 MOVNHW EMITW ;
: MOVK, {: rd imm sh :}  rd imm sh 16 / MOVKHW EMITW ;
: ADD,  {: rd rn rm :}  rd rn rm ENC-ADD EMITW ;
: ADDI, {: rd rn imm :}  rd rn imm ENC-ADDI EMITW ;
: SUB,  {: rd rn rm :}  rd rn rm ENC-SUB EMITW ;
: SUBI, {: rd rn imm :}  rd rn imm ENC-SUBI EMITW ;
: MUL,  {: rd rn rm :}  rd rn rm ENC-MUL EMITW ;
: SDIV, {: rd rn rm :}  rd rn rm ENC-SDIV EMITW ;
: UDIV, {: rd rn rm :}  rd rn rm ENC-UDIV EMITW ;
: AND,  {: rd rn rm :}  rd rn rm ENC-AND EMITW ;
: ORR,  {: rd rn rm :}  rd rn rm ENC-ORR EMITW ;
: EOR,  {: rd rn rm :}  rd rn rm ENC-EOR EMITW ;
: ANDI, {: rd rn nis :}  rd rn nis ENC-ANDI EMITW ;
: ORRI, {: rd rn nis :}  rd rn nis ENC-ORRI EMITW ;
: EORI, {: rd rn nis :}  rd rn nis ENC-EORI EMITW ;
: LSLI, {: rd rn sh :}  rd rn sh ENC-LSLI EMITW ;
: LSRI, {: rd rn sh :}  rd rn sh ENC-LSRI EMITW ;
: ASRI, {: rd rn sh :}  rd rn sh ENC-ASRI EMITW ;
: LSLV, {: rd rn rm :}  rd rn rm ENC-LSLV EMITW ;
: LSRV, {: rd rn rm :}  rd rn rm ENC-LSRV EMITW ;
: CMP,  {: rn rm :}  rn rm ENC-CMP EMITW ;
: CMPI, {: rn imm :}  rn imm ENC-CMPI EMITW ;
: CSET, {: rd cond :}  rd cond ENC-CSET EMITW ;
: LDR,  {: rt rn off :}  rt rn off ENC-LDR EMITW ;
: STR,  {: rt rn off :}  rt rn off ENC-STR EMITW ;
: LDRB, {: rt rn off :}  rt rn off ENC-LDRB EMITW ;
: STRB, {: rt rn off :}  rt rn off ENC-STRB EMITW ;
: LDRW, {: rt rn off :}  rt rn off ENC-LDRW EMITW ;
: STRW, {: rt rn off :}  rt rn off ENC-STRW EMITW ;
: SVC,  {: imm :}  imm ENC-SVC EMITW ;
: RET,  ENC-RET EMITW ;
: BLR,  {: rn :}  rn ENC-BLR EMITW ;
: BR,   {: rn :}  rn ENC-BR EMITW ;
: BRK,  ENC-BRK EMITW ;
: NOP,  ENC-NOP EMITW ;
: ICIVAU, {: rt :}  rt ENC-ICIVAU EMITW ;
: DCCVAU, {: rt :}  rt ENC-DCCVAU EMITW ;
: DSB-ISH,  ENC-DSB-ISH EMITW ;
: ISB,      ENC-ISB EMITW ;
