\ Cross-build two x86-64 comparison routines through the real HIR pass chain.
\ Run both images on the peer: each must exit 0 after checking true as -1 and
\ false as 0. The harness assigns each case a distinct failure status.
require test/x86-64-peer-image-fixture.f

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
   CC m X64HARNESS:POSITION NBACK:EMIT
   X64EMIT:BYTES X64EMIT:SIZE X64HARNESS:APPEND-ROUTINE
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

package X64COMPARE
using X64HARNESS
private

: CMP-ENTRY, ( bool -- ) {: folded:bool :}
   false OPEN,
   folded if
      20 7 -1 CASE2,
      -1000 0 0 CASE2,
      0 999 -1 CASE2,
      0 1000 0 CASE2,
   else
      7 20 -1 CASE2,
      20 7 0 CASE2,
      -1 0 -1 CASE2,
      0 -1 0 CASE2,
   then
   CLOSE, ENTRY, ;

: BUILD-CMP ( bool ptr u8 n -- ) {: folded:bool path:ptr pathu:n :}
   folded CMP-ENTRY,
   folded if X64CHAIN-TEST:PEER-CMP-IMM
   else X64CHAIN-TEST:PEER-CMP-REG then
   path pathu WRITE-ELF ;

public
: CMP-RUN ( -- )
   T-RESET
   INIT
   false s" hb-x64-compare-reg" TMP-PATH BUILD-CMP
   true s" hb-x64-compare-imm" TMP-PATH BUILD-CMP
   DISPOSE
   T-REPORT ;
;package

X64COMPARE:CMP-RUN
