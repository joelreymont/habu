\ Cross-build two x86-64 comparison routines through the real HIR pass chain.
\ Run both images on the peer: each must exit 0 after checking true as -1 and
\ false as 0. A case failure exits with its own status (61 through 68).
require test/x86-64-peer-image.f

package X64CHAIN-TEST
private

: CMP-CONST ( n -- IR-ID:ir-value-id ) {: v:n :}
   HIR-OPCODE:CONST BODY-ST BODY-LN OPEN-OP
   CC BB CELLT IR-BUILD:ADD-RESULT
   CC BB  CC BB HIR:KEY-VALUE  CC BB v IR-BUILD:INTERN-INT-ATTR
   IR-BUILD:ADD-ATTR
   CC BB  CC BB HIR:KEY-ADDR  CC BB HIR:ADDR-NONE HIR:ADDR-ATTR
   IR-BUILD:ADD-ATTR
   CLOSE-VALUE ;

: BUILD-CMP-REG ( -- )
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   HIR-OPCODE:LT a b BINOP RET1
   CLOSE-FUN ;

: BUILD-CMP-IMM ( -- )
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   HIR-OPCODE:SUB b a BINOP {: diff:IR-ID:ir-value-id :}
   HIR-OPCODE:LT diff 1000 CMP-CONST BINOP RET1
   CLOSE-FUN ;

: EMIT-CMP ( -- )
   2 1 CHAIN {: m:IR-BUILD:module :}
   CC m X64PEER:POSITION NBACK:EMIT
   X64EMIT:BYTES X64EMIT:SIZE X64PEER:APPEND-ROUTINE
   CC NBACK:RETIRE
   CC NBACK:RELEASE ;

: CMP-REG-BODY ( IR-CTX:ctx -- )
   HIR-MOD BUILD-CMP-REG EMIT-CMP ;

: CMP-IMM-BODY ( IR-CTX:ctx -- )
   HIR-MOD BUILD-CMP-IMM EMIT-CMP ;

public
: PEER-CMP-REG ( -- ) WBND [: CMP-REG-BODY ;] IR-CTX:WITH-CONTEXT ;
: PEER-CMP-IMM ( -- ) WBND [: CMP-IMM-BODY ;] IR-CTX:WITH-CONTEXT ;
;package

package X64PEER
using X64ASM
using X64CODE
private

: CMP-ENTRY, ( bool -- ) {: folded:bool :}
   ASM-RESET
   LBL EXIT-CELL !  LBL ROUTINE-CELL !
   RSP 1024 >IMM32 ASM-SINK ENC-SUB-RI32
   RBP RSP ASM-SINK ENC-MOV-RR
   folded if
      20 7 -1 65 CASE,
      -1000 0 0 66 CASE,
      0 999 -1 67 CASE,
      0 1000 0 68 CASE,
   else
      7 20 -1 61 CASE,
      20 7 0 62 CASE,
      -1 0 -1 63 CASE,
      0 -1 0 64 CASE,
   then
   RDI 0 IMM
   EXIT-LBL JMP,
   ROUTINE-OFF PAD-TO
   ROUTINE-LBL LBL, ;

: BUILD-CMP ( bool ptr u8 n -- ) {: folded:bool path:ptr pathu:n :}
   folded CMP-ENTRY,
   folded if X64CHAIN-TEST:PEER-CMP-IMM
   else X64CHAIN-TEST:PEER-CMP-REG then
   EXIT,
   ASM-CODE BUILD-IMAGE
   s" x64-peer" SET-SIGID CODESIG2
   path pathu DRV-WRITE-IMAGE
   s" ELF names x86-64 and enters the comparison code" T-LABEL
   $12 M-OFF M-LE32@ $FFFF and 62 T=
   $18 M-OFF M-LE32@ VMBASE CODE-OFF + T= ;

public
: CMP-RUN ( -- )
   T-RESET
   ASM-SINK CODE-CAP-BYTES BUF:N>BLEN BUF:INIT
   false s" hb-x64-compare-reg" TMP-PATH BUILD-CMP
   true s" hb-x64-compare-imm" TMP-PATH BUILD-CMP
   ASM-SINK BUF:DISPOSE
   T-REPORT ;
;package

X64PEER:CMP-RUN
