\ maki/cad.f - Model CAD REPL commands + checked MODEL: capture (dot cad-1).
\
\ MODEL: is a package-scoped colon definer (CAD-PLAN section 3). It does NOT parse op
\ tokens against a table, nor drive a per-token capture engine: it TRANSLATES the model
\ body into an ordinary checked ": ( tensor ... -- tensor ) ... ;" definition over the
\ named planning vocabulary (maki/plan-vocab.f, package PLAN), then a single nested
\ `evaluate` COMPILES that definition with the check hook active - so the checker
\ certifies the WHOLE composition's stack arity + tensor discipline at MODEL: time -
\ and RUNS it once against seeded descriptors to CAPTURE the plan (the vocab words append
\ IR nodes to the plan store exactly as the old capture engine did). The captured plan is
\ bridged into the model-IR node table (maki/model-ir.f), identical facts to before.
\
\ The translation is mechanical (op registry supplies each op's arity/class): the first
\ signature input is the running value; each op's parameter operands are drained FIFO from
\ pending named references then the declared-input cursor, and emitted as explicit stack
\ pushes before the vocab word; ">V NAME" compiles to `dup {: NAME:tensor :}` (name the
\ current value, keep it running); "RESHAPE:RxC"/"SLICE:R0..R1" rewrite to the vocab's
\ ( tensor n n -- tensor ) forms with the scalar params. A malformed composition - an op
\ with too few operands (arity underflow), a non-tensor operand - is now a CHECKER
\ diagnostic (load-time exit 70) from the nested compile, not a runtime E-CAD-ARITY throw.
\
\ Surface (single line):  MODEL: NAME ( x:RxC w:RxC ... -- y ) OP OP ... ;
\ Inputs are declared with shapes; the first is the running "current" tensor and
\ each op consumes it plus (arity-1) further declared inputs as parameters. Shapes
\ bind now (the OPTIMIZE-time shape binding of docs/model-cad.md is a later dot);
\ an unbound extent may be written 0 and renders "?".
\
\ Named value references (v1 DAG seam) - a body may NAME a value and reference it as
\ a later op's operand, so an op reads an EARLIER intermediate or a declared input
\ instead of the next positional input (true residual / skip connections, fan-out):
\   - a signature input name (the "x" in "x:RxC") binds to that input's value;
\   - ">V NAME" binds NAME to the CURRENT value (the last op's output);
\   - a bare NAME token pushes that value as the NEXT op's parameter operand (FIFO
\     across a multi-operand op), instead of consuming the next declared input.
\ So "LINEAR GELU LINEAR x RESIDUAL-ADD RMSNORM" adds the ORIGINAL input x back (a
\ real skip), and ">V H1 ... H1 RESIDUAL-ADD" fans a named intermediate out to two
\ consumers. The translator resolves references at MODEL: time by name; the compiled
\ body threads them as explicit stack operands so the checker sees the whole DAG.
\
\ Fail closed: unknown op / unbound reference token -> E-CAD-OP; empty body ->
\ E-CAD-EMPTY; malformed signature/shape -> E-CAD-SYNTAX; too many inputs -> E-CAD-INPUTS;
\ a bad named value (duplicate, op-shadow, oversized, table full) -> E-CAD-NAME; ">V" with
\ no current value -> E-CAD-NOVALUE; a reference the following op cannot accept ->
\ E-CAD-REF. An op with too few operands (arity underflow) is a CHECKER diagnostic from
\ the nested compile (load-time exit 70), no longer a runtime E-CAD-ARITY throw. A param
\ operand whose shape is illegal for the op's broadcast class
\ (add/mul/residual-add need same-shape, bias 1xC, scale 1x1 or same-shape, linear
\ bias 1xN) -> E-CAD-PARAM-SHAPE (an unbound 0 extent defers to BIND-SHAPES reprop).
\ LOWER reports REAL node facts (op
\ count + shape/dtype/layout keys from the IR); FUSE plans real regions + traffic
\ (maki/fusion-plan.f, maki/traffic.f); MEMORY plans per-hot coalescing status +
\ vector-width/tail facts (maki/mem-plan.f, cad-3); TILE stays conservative (cad-4).
\ GOLDEN/GRADCHECK are REAL on the host now; PROFILE stays honest not-run without a
\ GPU. PROMOTE (CAD 7c gate set) refuses (E-CAD-GATE) unless CERTIFY passes, GOLDEN
\ passes, and GRADCHECK did not FAIL (not-run clears it); PROFILE is mandatory-to-run
\ but never blocks. maki -> habu only; cad owns -5020..-5029.

require lib/string.f
require lib/float.f                 \ POW10: MODEL:/GOLDEN drives the float-tolerance golden compare
require maki/report.f
require maki/op-kind.f
require maki/op-registry.f
require maki/move-facts.f
require maki/tensor-value.f
require maki/plan-ops.f
require maki/plan-vocab.f            \ the package-scoped, descriptor-typed model vocabulary MODEL: compiles over
require maki/model-ir.f
require maki/fusion-plan.f
require maki/traffic.f
require maki/mem-plan.f
require maki/schedule.f
require maki/sched-key.f
require maki/store.f
require maki/golden.f
require maki/lower-golden.f
require maki/gradcheck.f

-5020 constant E-CAD-NOMODEL   \ command issued with no model defined
-5021 constant E-CAD-OP        \ unknown op token in a MODEL: body
-5022 constant E-CAD-EMPTY     \ MODEL: with no name or no ops
-5023 constant E-CAD-GATE      \ PROMOTE refused: required gates did not pass
-5024 constant E-CAD-SYNTAX    \ malformed MODEL: (bad signature / shape / terminator)
-5025 constant E-CAD-ARITY     \ RETIRED: arity underflow is now a nested-compile checker diagnostic (kept: reserved slot)
-5026 constant E-CAD-INPUTS    \ more model inputs than the capture pool holds
-5027 constant E-CAD-NAME      \ bad named value: duplicate, op-shadow, oversized, or table full
-5028 constant E-CAD-NOVALUE   \ ">V" naming with no current value to name
-5029 constant E-CAD-REF       \ a named reference the following op cannot accept
\ OPTIMIZE-time shape binding (BIND-SHAPES) uses the free -5160..-5162 slice.
-5160 constant E-CAD-BIND-COUNT    \ BIND-SHAPES spec count != model input slots
-5161 constant E-CAD-BIND-CONFLICT \ a spec contradicts an already-bound (nonzero) extent
-5162 constant E-CAD-BIND-SHAPE    \ malformed/zero spec dim or illegal re-propagated shape
\ Capture-time param-operand shape legality uses -5163 (the -502x capture decade is full).
-5163 constant E-CAD-PARAM-SHAPE   \ elementwise/linear param operand shape illegal for the op's broadcast class

package MAKI
private

variable MODEL-SET?                        \ 0 until a MODEL: succeeds
                                           \ model NAME lives in the IR (MIR-NAME$)

64 constant CAP-CAP                        \ max model inputs (matches model-ir slots)
create CAP-INS CAP-CAP cells allot         \ declared input tensor handles, order 0..N-1
variable CAP-IN-N                          \ number of declared inputs
variable CAP-IP                            \ declared-input cursor: next input a param may drain (starts 1)
variable CAP-OPS                           \ ops emitted so far (a running value exists once >0)

\ name set: signature input names occupy NT slots 0..N-1 (slot index == input index, so a
\ by-name reference and the declared-input cursor resolve to the SAME compiled local); ">V"
\ intermediates occupy the slots above. A bare body token that hits the set is a value
\ reference (a parameter for the next op); anything else is an op token.
96 constant NT-CAP                         \ names: up to CAP-CAP inputs + ">V" intermediates
16 constant NT-NAME-CAP                    \ max bytes per name
create NT-NAMES NT-CAP NT-NAME-CAP * allot \ fixed-width name text slots
create NT-LENS  NT-CAP cells allot         \ name lengths
variable NT-N

\ pending reference queue: NT slot indices drained (FIFO) into the next op's parameter slots
4 constant CAP-PEND-CAP                    \ max pending references before one op
create CAP-PEND CAP-PEND-CAP cells allot   \ pending reference NT slot indices
variable CAP-PEND-N                        \ tail (push index)
variable CAP-PEND-HD                       \ head (dequeue index)

\ compiled-body source buffer: MODEL: emits a checked ": ... ;" over package PLAN here, then
\ one nested `evaluate` compiles it (the checker certifies the whole composition) and runs it
\ once to capture the plan. Sized for the largest single-line model body.
2048 constant MSRC-CAP
create MSRC-BUF MSRC-CAP allot
variable MSRC-N

\ generated throwaway name for the capture word (redefinition is fatal, so each MODEL: mints a
\ fresh %MDLCAP<ctr>); the compiled word is run once and left defined (harmless, uniquely named).
create CAPNAME-BUF 24 allot
variable CAPNAME-N
variable MDL-CTR

public

\ ---- op-name -> op-kind (fail closed: unknown op is rejected, never guessed) -
\ OP-LOOKUP is the non-throwing table (op valid only when the flag is true); OP-KIND
\ wraps it and throws E-CAD-OP on an unknown token. The non-throwing form lets the
\ name table reject a value name that would shadow a reserved op token.
: OP-LOOKUP ( ptr u8 n -- n bool )
   2dup s" ADD"          STR= if 2drop OP-ADD          true exit then
   2dup s" MUL"          STR= if 2drop OP-MUL          true exit then
   2dup s" SCALE"        STR= if 2drop OP-SCALE        true exit then
   2dup s" BIAS"         STR= if 2drop OP-BIAS         true exit then
   2dup s" RELU"         STR= if 2drop OP-RELU         true exit then
   2dup s" GELU"         STR= if 2drop OP-GELU         true exit then
   2dup s" SILU"         STR= if 2drop OP-SILU         true exit then
   2dup s" LAYERNORM"    STR= if 2drop OP-LAYERNORM    true exit then
   2dup s" RMSNORM"      STR= if 2drop OP-RMSNORM      true exit then
   2dup s" SOFTMAX-ROW"  STR= if 2drop OP-SOFTMAX-ROW  true exit then
   2dup s" MATMUL"       STR= if 2drop OP-MATMUL       true exit then
   2dup s" LINEAR"       STR= if 2drop OP-LINEAR       true exit then
   2dup s" RESIDUAL-ADD" STR= if 2drop OP-RESIDUAL-ADD true exit then
   2dup s" CAST"         STR= if 2drop OP-CAST         true exit then
   2dup s" ROPE"         STR= if 2drop OP-ROPE         true exit then
   2dup s" RESHAPE"      STR= if 2drop OP-RESHAPE      true exit then
   2dup s" TRANSPOSE"    STR= if 2drop OP-TRANSPOSE    true exit then
   2dup s" SLICE"        STR= if 2drop OP-SLICE        true exit then
   2dup s" CONCAT"       STR= if 2drop OP-CONCAT       true exit then
   2dup s" GATHER"       STR= if 2drop OP-GATHER       true exit then
   2drop 0 false ;

: OP-KIND ( ptr u8 n -- n )
   OP-LOOKUP 0= if E-CAD-OP throw then ;

private

\ ---- compiled-body source buffer (build the checked ": ... ;" MODEL: compiles) ------
: MSRC-RESET ( -- )  0 MSRC-N ! ;
: MSRC+ ( ptr u8 n -- ) {: a:ptr u:n :}
   MSRC-N @ u + MSRC-CAP > if E-CAD-SYNTAX throw then
   a  MSRC-BUF MSRC-N @ +  u  BYTE-COPY
   MSRC-N @ u + MSRC-N ! ;
: MSRC-C ( n -- ) {: c:n :}
   MSRC-N @ 1+ MSRC-CAP > if E-CAD-SYNTAX throw then
   c  MSRC-BUF MSRC-N @ +  c!  MSRC-N @ 1+ MSRC-N ! ;
: MSRC-SP ( -- )  $20 MSRC-C ;
: MSRC-INT ( n -- )  SB-RESET SB-INT SB$ MSRC+ ;   \ decimal text via the shared SB, copied stable
: MSRC$ ( -- ptr u8 n )  MSRC-BUF MSRC-N @ ;

\ ---- pending reference queue (FIFO NT slot indices; the next op drains them into params) --
: CAP-PEND-RESET ( -- )  0 CAP-PEND-N !  0 CAP-PEND-HD ! ;
: CAP-PEND-CNT ( -- n )  CAP-PEND-N @ CAP-PEND-HD @ - ;      \ remaining pending refs
: CAP-PEND-PUSH ( n -- )
   CAP-PEND-N @ CAP-PEND-CAP >= if E-CAD-REF throw then
   CAP-PEND-N @ cells CAP-PEND + !  CAP-PEND-N @ 1+ CAP-PEND-N ! ;
: CAP-PEND-DEQ ( -- n )
   CAP-PEND-HD @ cells CAP-PEND + @  CAP-PEND-HD @ 1+ CAP-PEND-HD ! ;

\ ---- name set (input names slots 0..N-1, then ">V" names; slot index is the ref handle) --
: NT-RESET ( -- )  0 NT-N ! ;
: NT-SLOT ( n -- ptr u8 )  NT-NAME-CAP *  NT-NAMES + ;
: NT-NAME ( n -- ptr u8 n ) {: i:n :}  i NT-SLOT  i cells NT-LENS + @ ;   \ slot -> name text
: NT-FIND ( ptr u8 n -- n bool ) {: a:ptr u:n :}   \ slot index valid only when true
   NT-N @ 0 ?do
      a u  i NT-NAME  STR= if  i true  unloop exit  then
   loop  0 false ;
: NT-BIND ( ptr u8 n -- n ) {: a:ptr u:n :}        \ intern a name, return its slot index
   u NT-NAME-CAP > if E-CAD-NAME throw then                 \ name too long
   a u OP-LOOKUP nip if E-CAD-NAME throw then               \ a name may not shadow an op token
   a u NT-FIND   nip if E-CAD-NAME throw then               \ no duplicate name
   NT-N @ NT-CAP >= if E-CAD-NAME throw then                \ table full
   NT-N @ {: i:n :}
   a  i NT-SLOT  u  BYTE-COPY
   u  i cells NT-LENS + !
   i 1+ NT-N !  i ;

\ ---- capture / translate state ---------------------------------------------
: CAP-HAS-VALUE? ( -- bool )  CAP-IN-N @ 0<>  CAP-OPS @ 0<> or ;   \ a running value exists

: CAP-BEGIN ( -- )
   TENSOR:TV-RESET  TENSOR:PLAN-RESET  MIR-RESET
   0 CAP-IN-N !  1 CAP-IP !  0 CAP-OPS !
   NT-RESET  CAP-PEND-RESET  MSRC-RESET
   0 MODEL-SET? ! ;

\ fresh throwaway capture-word name -> CAPNAME-BUF (redefinition is fatal, so mint per MODEL:)
: CAP-GEN-NAME ( -- )
   SB-RESET  s" %MDLCAP" SB-APPEND  MDL-CTR @ SB-INT  MDL-CTR @ 1+ MDL-CTR !
   SB$ {: a:ptr u:n :}  a CAPNAME-BUF u BYTE-COPY  u CAPNAME-N ! ;
: CAPNAME$ ( -- ptr u8 n )  CAPNAME-BUF CAPNAME-N @ ;

\ ---- body emit helpers (translate one op's operands into explicit stack pushes) ------
\ emit the declared input at the cursor (advancing it); exhausted -> emit nothing so the op
\ underflows and the nested checker rejects it (arity underflow is a checker diagnostic).
: CAP-EMIT-DECL ( -- )
   CAP-IP @ CAP-IN-N @ < if
      CAP-IP @ NT-NAME MSRC+ MSRC-SP  CAP-IP @ 1+ CAP-IP !
   then ;

\ emit k parameter operands: pending references first (FIFO), then declared inputs
: CAP-EMIT-PARAMS ( n -- ) {: k:n :}
   CAP-PEND-CNT k > if E-CAD-REF throw then        \ more refs than the op accepts (e.g. ref before a unary op)
   k 0 ?do
      CAP-PEND-CNT 0 > if  CAP-PEND-DEQ NT-NAME MSRC+ MSRC-SP
      else  CAP-EMIT-DECL  then
   loop ;

\ emit the vocabulary word for a body op token: "PLAN:<TOKEN> "
: CAP-EMIT-OP ( ptr u8 n -- ) {: a:ptr u:n :}
   s" PLAN:" MSRC+  a u MSRC+  MSRC-SP  1 CAP-OPS +! ;

\ ">V NAME": name the current running value, keep it running -> compile `dup {: NAME:tensor :}`
: NT-BIND-CUR ( ptr u8 n -- ) {: a:ptr u:n :}
   CAP-HAS-VALUE? 0= if E-CAD-NOVALUE throw then
   CAP-PEND-CNT 0 > if E-CAD-REF throw then         \ a ref must be consumed before naming
   a u NT-BIND drop
   s" dup {: " MSRC+  a u MSRC+  s" :tensor :} " MSRC+ ;

\ ---- param-operand shape legality (shared by capture + BIND-SHAPES reprop) ------
\ A binary elementwise op's parameter must broadcast-match its data operand under the
\ op's documented class (the same classes maki/backward.f adjoints assume):
\   ADD / MUL / RESIDUAL-ADD : param EQUALS data
\   BIAS                     : param is 1 x (data cols)      (row broadcast)
\   SCALE                    : param EQUALS data OR is 1 x 1  (scalar broadcast)
\ LINEAR routes its bias through the BIAS class against the OUTPUT cols. An unbound
\ (0) extent defers: capture passes and BIND-SHAPES re-propagation re-checks once
\ bound. Ops with no documented class (rope / synthesized backward ops) are unconstrained.
: SHP-BOUND? ( CAD-KIND:rows CAD-KIND:cols CAD-KIND:rows CAD-KIND:cols -- bool )
   {: dr:CAD-KIND:rows dc:CAD-KIND:cols pr:CAD-KIND:rows pc:CAD-KIND:cols :}
   dr 0 ROWS-IS? 0= dc 0 COLS-IS? 0= and
   pr 0 ROWS-IS? 0= and pc 0 COLS-IS? 0= and ;
: SHP-SAME? ( CAD-KIND:rows CAD-KIND:cols CAD-KIND:rows CAD-KIND:cols -- bool )
   SHAPE-EQUAL? ;
: SHP-ROW? ( CAD-KIND:cols CAD-KIND:rows CAD-KIND:cols -- bool )
   {: dc:CAD-KIND:cols pr:CAD-KIND:rows pc:CAD-KIND:cols :}
   pr 1 ROWS-IS? pc dc COLS-EQUAL? and ;
: SHP-SCALAR? ( CAD-KIND:rows CAD-KIND:cols -- bool )
   {: pr:CAD-KIND:rows pc:CAD-KIND:cols :}
   pr 1 ROWS-IS? pc 1 COLS-IS? and ;
: SHP-LEGAL? ( CAD-KIND:rows CAD-KIND:cols CAD-KIND:rows CAD-KIND:cols n -- bool )
   {: dr:CAD-KIND:rows dc:CAD-KIND:cols pr:CAD-KIND:rows pc:CAD-KIND:cols op:n :}
   dr dc pr pc SHP-BOUND? 0= if true exit then             \ unbound -> defer to reprop
   op OP-BIAS = if dc pr pc SHP-ROW? exit then
   op OP-SCALE = if dr dc pr pc SHP-SAME?  pr pc SHP-SCALAR? or exit then
   op OP-ADD = op OP-MUL = or op OP-RESIDUAL-ADD = or if
      dr dc pr pc SHP-SAME? exit then
   true ;                                                  \ op has no documented class
: SHP-CHECK ( CAD-KIND:rows CAD-KIND:cols CAD-KIND:rows CAD-KIND:cols n -- )
   SHP-LEGAL? 0= if E-CAD-PARAM-SHAPE throw then ;

\ capture entry points: elementwise param vs its data operand; linear bias vs output cols
: EW-SHAPE-CHECK ( tensor tensor n -- ) {: x:tensor p:tensor op:n :}
   x TENSOR:TV-ROWS@ x TENSOR:TV-COLS@  p TENSOR:TV-ROWS@ p TENSOR:TV-COLS@  op  SHP-CHECK ;
: LIN-BIAS-CHECK ( tensor tensor tensor -- ) {: x:tensor w:tensor b:tensor :}
   x TENSOR:TV-ROWS@ w TENSOR:TV-COLS@  b TENSOR:TV-ROWS@ b TENSOR:TV-COLS@  OP-BIAS  SHP-CHECK ;

\ ---- post-capture param-operand shape legality (over the captured plan store) --------
\ The named vocabulary is the arity/kind surface only (plan-vocab.f); broadcast legality
\ layers here, exactly as the old per-op capture did. After the compiled body runs and
\ populates the plan store, each elementwise/linear node's parameter operand is re-checked
\ against its data operand's shape. An unbound (0) extent defers to BIND-SHAPES reprop.
: PLAN-SHP-NODE ( n -- ) {: j:n :}
   j TENSOR:PLAN-OP@ {: op:n :}
   op OPR-CLASS {: cls:n :}
   cls CLASS-EW = if
      j TENSOR:PLAN-IN-COUNT@ 2 < if exit then                       \ unary elementwise: no param operand
      j 0 TENSOR:PLAN-IN@  j 1 TENSOR:PLAN-IN@  op EW-SHAPE-CHECK  exit then
   cls CLASS-MATMUL = if
      j TENSOR:PLAN-IN-COUNT@ 3 < if exit then                       \ matmul (no bias operand): skip
      j 0 TENSOR:PLAN-IN@  j 1 TENSOR:PLAN-IN@  j 2 TENSOR:PLAN-IN@  LIN-BIAS-CHECK then ;
: PLAN-SHP-ALL ( -- )  TENSOR:PLAN-N@ 0 ?do  i PLAN-SHP-NODE  loop ;

\ ---- bridge the captured plan into the model-IR node table -----------------
: CAP-IN-A ( -- ptr tensor ) CAP-INS ;
: CAP-IN! ( tensor n -- ) cells CAP-IN-A + ! ;
: CAP-IN@ ( n -- tensor ) cells CAP-IN-A + @ ;
: CAP-IN-FIND ( tensor -- n bool ) {: t:tensor :}
   CAP-IN-N @ 0 ?do
      t i CAP-IN@ TENSOR:TV-EQUAL? if i true unloop exit then
   loop
   0 false ;
: PLAN-OUT-FIND ( tensor -- n bool ) {: t:tensor :}
   TENSOR:PLAN-N@ 0 ?do
      t i TENSOR:PLAN-OUT@ TENSOR:TV-EQUAL? if i true unloop exit then
   loop
   0 false ;
: PLAN-REF ( tensor -- MIR:operand-ref ) {: t:tensor :}
   t CAP-IN-FIND if MIR-SLOT-ID MIR-IN-REF exit then drop
   t PLAN-OUT-FIND if MIR-NODE-ID MIR-NODE-REF exit then drop
   E-CAD-REF throw ;

\ movement nodes materialize only on a materialize/gathered verdict; compute nodes
\ stay materialized (the conservative cad-1 default until the fusion planner lands).
: BRIDGE-MAT ( n n -- n ) {: op:n attr:n :}     \ op-kind attr -> materialization flag
   op OPR-CLASS CLASS-MOVEMENT = if
      attr MV-VD@ MV-VD-REPORTS? if 1 else 0 then
   else 1 then ;

: BRIDGE-NODE ( n -- ) {: j:n :}
   j TENSOR:PLAN-OP@ {: op:n :}
   op MIR-OP-BEGIN
   j TENSOR:PLAN-IN-COUNT@ 0 ?do  j i TENSOR:PLAN-IN@ PLAN-REF MIR-IN+  loop
   j TENSOR:PLAN-OUT@ {: y:tensor :}
   j TENSOR:PLAN-ATTR@ {: attr:n :}
   y TENSOR:TV-ROWS@ y TENSOR:TV-COLS@ y TENSOR:TV-DTYPE@ y TENSOR:TV-LAYOUT@  attr  op attr BRIDGE-MAT  MIR-OP+ drop ;

: BRIDGE-PLAN ( -- )  TENSOR:PLAN-N@ 0 ?do i BRIDGE-NODE loop ;

\ finish capture after the compiled body ran: no dangling reference, a non-empty plan, the
\ param-operand shape pass (E-CAD-PARAM-SHAPE), then bridge the plan into the model IR.
: CAP-FINISH ( -- )
   CAP-PEND-CNT 0 > if E-CAD-REF throw then         \ a named ref left unconsumed by any op
   TENSOR:PLAN-N@ 0= if E-CAD-EMPTY throw then             \ the compiled body captured no ops
   PLAN-SHP-ALL
   BRIDGE-PLAN
   -1 MODEL-SET? ! ;

\ ---- MODEL: signature + body parser ----------------------------------------
: PARSE-INT ( ptr u8 n -- n )
   STR>NUMBER? 0= if E-CAD-SYNTAX throw then ;

: PARSE-SHAPE ( ptr u8 n -- n n ) {: a:ptr u:n :}   \ "name:RxC" or "RxC" -> rows cols
   a u $3A INDEX-OF {: ci:n :}
   ci 0< if 0 else ci 1+ then {: off:n :}       \ shape span starts past any "name:"
   a off +  u off -  $78 INDEX-OF {: xi:n :}     \ 'x' index within the shape span
   xi 0< if E-CAD-SYNTAX throw then
   a off +          xi           PARSE-INT       \ rows
   a off + xi 1+ +  u off - xi 1+ -  PARSE-INT ; \ cols

: SKIP-TO-RPAREN ( -- )                         \ swallow output names up to ')'
   begin
      parse-name dup 0= if 2drop E-CAD-SYNTAX throw then
      s" )" STR= if exit then
   again ;

\ the name span of a "name:RxC" spec (empty when the spec is a bare "RxC")
: SPEC-NAME ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u $3A INDEX-OF {: ci:n :}
   ci 0< if a 0 else a ci then ;

\ one "[name:]RxC" spec: build the input descriptor + IR slot, and intern its local name
\ (synthesized when the spec is a bare "RxC") into NT slot == input index.
: CAP-SYNTH-NAME ( -- )                             \ bare RxC input: bind IN<idx>
   SB-RESET  s" IN" SB-APPEND  CAP-IN-N @ SB-INT  SB$ NT-BIND drop ;
: SIG-INPUT ( ptr u8 n -- ) {: a:ptr u:n :}
   CAP-IN-N @ CAP-CAP >= if E-CAD-INPUTS throw then
   a u PARSE-SHAPE {: rows:n cols:n :}
   rows cols SHAPE DT-F32 LAY-ROW SPACE-HOST TENSOR:TV-DESC
   CAP-IN-N @ CAP-IN!
   rows cols SHAPE DT-F32 LAY-ROW MIR-INPUT+ drop
   a u SPEC-NAME {: na:ptr nu:n :}
   nu 0 > if  na nu NT-BIND drop  else  CAP-SYNTH-NAME  then
   CAP-IN-N @ 1+ CAP-IN-N ! ;

: PARSE-SIG ( -- )                              \ '(' input-specs [ -- names ] ')'
   parse-name dup 0= if 2drop E-CAD-SYNTAX throw then
   s" (" STR= 0= if E-CAD-SYNTAX throw then
   begin
      parse-name dup 0= if 2drop E-CAD-SYNTAX throw then
      2dup s" --" STR= if 2drop SKIP-TO-RPAREN exit then
      2dup s" )"  STR= if 2drop exit then
      SIG-INPUT
   again ;

: PARSE-RANGE ( ptr u8 n -- n n ) {: a:ptr u:n :}   \ "R0..R1" -> r0 r1
   a u $2E INDEX-OF {: di:n :}                       \ first '.'
   di 0< di 1+ u >= or if E-CAD-SYNTAX throw then
   a di 1+ + c@ $2E <> if E-CAD-SYNTAX throw then     \ require the second '.'
   a di            PARSE-INT                          \ r0
   a di 2 + +  u di 2 + -  PARSE-INT ;                \ r1

: EMIT-ROW-PARAM ( n -- )
   MSRC-INT s"  1 MAKI:SHAPE drop " MSRC+ ;

\ "MOVE:params" body token (RESHAPE:RxC | SLICE:R0..R1): emit nominal params then the word.
: EMIT-MOVE-PARAM ( ptr u8 n ptr u8 n -- ) {: op:ptr opu:n pa:ptr pu:n :}
   op opu OP-KIND {: mop:n :}
   mop OP-RESHAPE = if  pa pu PARSE-SHAPE  swap MSRC-INT MSRC-SP MSRC-INT MSRC-SP
      s" MAKI:SHAPE " MSRC+
      op opu CAP-EMIT-OP  exit  then
   mop OP-SLICE   = if  pa pu PARSE-RANGE swap EMIT-ROW-PARAM EMIT-ROW-PARAM
      op opu CAP-EMIT-OP  exit  then
   E-CAD-SYNTAX throw ;                              \ only reshape/slice carry ':' params

\ one body op token (not a ">V" and not a bare reference): translate to explicit stack form.
: EMIT-OP-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   a u $3A INDEX-OF {: ci:n :}
   ci 0< if
      a u OP-KIND {: op:n :}
      op OP-RESHAPE = op OP-SLICE = or if E-CAD-SYNTAX throw then  \ reshape/slice need ":params"
      op OPR-ARITY 1- CAP-EMIT-PARAMS                              \ tensor params = arity-1
      a u CAP-EMIT-OP  exit  then
   a ci  a ci 1+ +  u ci 1+ -  EMIT-MOVE-PARAM ;                   \ "OP:params"

\ one body token: a known name is a pending reference; else an op token to translate
: CAP-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NT-FIND if  CAP-PEND-PUSH  exit  then  drop
   a u EMIT-OP-TOKEN ;

\ ">V NAME": name the current value (read the name token from the body)
: PARSE-NAMED ( -- )
   parse-name dup 0= if 2drop E-CAD-SYNTAX throw then  NT-BIND-CUR ;

: PARSE-BODY ( -- )                             \ op / ">V NAME" / reference tokens up to ';'
   begin
      parse-name dup 0= if 2drop E-CAD-SYNTAX throw then
      2dup s" ;"  STR= if 2drop exit then
      2dup s" >V" STR= if 2drop PARSE-NAMED else CAP-TOKEN then
   again ;

\ ---- assemble + compile + run the capture definition -----------------------
\ CAP-EMIT-SIG: "( tensor.. -- tensor ) {: <locals> :} <input0> " - the effect, typed locals,
\ and initial running-value push shared by the compile framing (MODEL:) and any dry-run framing.
: CAP-EMIT-SIG ( -- )
   s"  ( " MSRC+
   CAP-IN-N @ 0 ?do  s" tensor " MSRC+  loop
   s" -- tensor ) " MSRC+
   CAP-IN-N @ 0 > if
      s" {: " MSRC+
      CAP-IN-N @ 0 ?do  i NT-NAME MSRC+  s" :tensor " MSRC+  loop
      s" :} " MSRC+
      0 NT-NAME MSRC+  MSRC-SP                     \ push input0 as the initial running value
   then ;
\ CAP-EMIT-PREFIX: ": <cap> ( ... ) {: ... :} <input0>" up to the body.
: CAP-EMIT-PREFIX ( -- )  s" : " MSRC+  CAPNAME$ MSRC+  CAP-EMIT-SIG ;
: CAP-EMIT-SUFFIX ( -- )  s" ; " MSRC+  CAPNAME$ MSRC+ ;

\ seed the declared input descriptors onto the stack and run the assembled definition: the
\ nested `evaluate` compiles it (the check hook certifies the whole composition) then, in the
\ same string, runs the capture word once to populate the plan store. TRUSTED: `evaluate`
\ and the dynamic-arity descriptor push are metaprogramming the checker cannot express;
\ the compiled body is fully checked by the active hook (dot habu-checker-reentrancy-certify).
TRUSTED: CAP-COMPILE-RUN ( -- )
   CAP-IN-N @ 0 ?do i CAP-IN@ loop
   MSRC$ evaluate                                  \ compile + run the capture word
   drop ;                                          \ discard the captured output descriptor

public

\ MODEL: NAME ( inputs -- outputs ) OP OP ... ;   (single line)
\ Translates the body into a checked ": ... ;" over package PLAN, compiles it (the checker
\ certifies the whole composition), runs it once to capture the plan, then bridges to the IR.
: MODEL: ( -- )
   CAP-BEGIN  CAP-GEN-NAME
   parse-name dup 0= if 2drop E-CAD-EMPTY throw then MIR-NAME!
   PARSE-SIG
   CAP-EMIT-PREFIX
   PARSE-BODY
   CAP-EMIT-SUFFIX
   CAP-COMPILE-RUN
   CAP-FINISH ;

: MODEL-CLEAR ( -- )  CAP-BEGIN ;
: MODEL-DEFINED? ( -- bool )  MODEL-SET? @ 0= 0= ;
: MODEL-NAME$ ( -- ptr u8 n )  MIR-NAME$ ;
: MODEL-K ( -- n )  MIR-N@ ;

private

\ ---- OPTIMIZE-time shape re-propagation over the committed IR node table -----
\ Each node's output extents are a pure function of its operands' CURRENT extents
\ and its op class - the plan-ops.f inference rules re-expressed over IR nodes so a
\ rebind updates the whole downstream cone (CAD-PLAN section 13). Elementwise and
\ row-reduce forward ops keep the data operand's shape; matmul/linear take rows from
\ the data operand and cols from the weight (inner dim must agree); each movement op
\ recomputes its extents from its attrs and re-derives its dissolution verdict.
: RB-REF-ROWS ( MIR:operand-ref -- CAD-KIND:rows ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-ROWS@ else r MIR-REF-NODE MIR-ROWS@ then ;
: RB-REF-COLS ( MIR:operand-ref -- CAD-KIND:cols ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-COLS@ else r MIR-REF-NODE MIR-COLS@ then ;
: RB-REF-LAY ( MIR:operand-ref -- CAD-KIND:layout ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-LAY@ else r MIR-REF-NODE MIR-LAY@ then ;

\ elementwise / row-reduce forward: output = data operand (operand 0) shape
: RB-DATA ( CAD-KIND:node-id -- CAD-KIND:rows CAD-KIND:cols ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: r:MIR:operand-ref :}
   r RB-REF-ROWS  r RB-REF-COLS ;

\ contraction: rows from data operand, cols from weight; inner dim must agree
: RB-MM ( CAD-KIND:node-id -- CAD-KIND:rows CAD-KIND:cols ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: xr:MIR:operand-ref :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: wr:MIR:operand-ref :}
   xr RB-REF-COLS wr RB-REF-ROWS INNER-EQUAL? 0= if E-CAD-BIND-SHAPE throw then
   xr RB-REF-ROWS  wr RB-REF-COLS ;

\ movement extents per op (attrs carry reshape target / slice range; the rest come
\ from operand extents). Each binds its locals at entry (no branch-local rebinds).
: RB-RESHAPE ( CAD-KIND:node-id -- CAD-KIND:rows CAD-KIND:cols ) {: nd:CAD-KIND:node-id :}
   nd MIR-ATTR@ {: attr:n :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: r0:MIR:operand-ref :}
   attr MV-PA@ {: tr:n :}  attr MV-PB@ {: tc:n :}
   r0 RB-REF-ROWS r0 RB-REF-COLS SHAPE-ELEMS
   tr tc SHAPE SHAPE-ELEMS DIM-EQUAL? 0= if E-CAD-BIND-SHAPE throw then
   tr tc SHAPE ;
: RB-TRANSPOSE ( CAD-KIND:node-id -- CAD-KIND:rows CAD-KIND:cols ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: r0:MIR:operand-ref :}
   r0 RB-REF-ROWS r0 RB-REF-COLS TRANSPOSE-SHAPE ;
: RB-SLICE ( CAD-KIND:node-id -- CAD-KIND:rows CAD-KIND:cols ) {: nd:CAD-KIND:node-id :}
   nd MIR-ATTR@ {: attr:n :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: r0:MIR:operand-ref :}
   attr MV-PA@ {: a:n :}  attr MV-PB@ {: b:n :}
   a 0 < b r0 RB-REF-ROWS ROWS-RAW > or a b > or if E-CAD-BIND-SHAPE throw then
   b a - r0 RB-REF-COLS COLS-RAW SHAPE ;
: RB-CONCAT ( CAD-KIND:node-id -- CAD-KIND:rows CAD-KIND:cols ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: r0:MIR:operand-ref :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: r1:MIR:operand-ref :}
   r0 RB-REF-COLS r1 RB-REF-COLS COLS-EQUAL? 0= if E-CAD-BIND-SHAPE throw then
   r0 RB-REF-ROWS ROWS-RAW r1 RB-REF-ROWS ROWS-RAW +
   r0 RB-REF-COLS COLS-RAW SHAPE ;
: RB-GATHER ( CAD-KIND:node-id -- CAD-KIND:rows CAD-KIND:cols ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ {: r0:MIR:operand-ref :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: r1:MIR:operand-ref :}
   r1 RB-REF-ROWS r1 RB-REF-COLS SHAPE-ELEMS DIM-RAW
   r0 RB-REF-COLS COLS-RAW SHAPE ;

: RB-MOVE-SHAPE ( CAD-KIND:node-id -- CAD-KIND:rows CAD-KIND:cols ) {: nd:CAD-KIND:node-id :}
   nd MIR-OP@ {: op:n :}
   op OP-RESHAPE   = if nd RB-RESHAPE   exit then
   op OP-TRANSPOSE = if nd RB-TRANSPOSE exit then
   op OP-SLICE     = if nd RB-SLICE     exit then
   op OP-CONCAT    = if nd RB-CONCAT    exit then
   op OP-GATHER    = if nd RB-GATHER    exit then
   E-CAD-BIND-SHAPE throw ;

\ movement dissolution verdict re-derived from the new extents (slice re-checks its
\ offset/col alignment; the rest are layout- or constant-determined).
: RB-VD-RESHAPE ( CAD-KIND:node-id -- n ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ RB-REF-LAY MV-RESHAPE-VERDICT ;
: RB-VD-SLICE ( CAD-KIND:node-id CAD-KIND:cols -- n )
   {: nd:CAD-KIND:node-id cols:CAD-KIND:cols :}
   nd 0 MIR-INPUT-IDX MIR-IN@ RB-REF-LAY
   nd MIR-ATTR@ MV-PA@ cols COLS-RAW MV-SLICE-VERDICT ;
: RB-MOVE-VD ( CAD-KIND:node-id CAD-KIND:cols -- n )
   {: nd:CAD-KIND:node-id cols:CAD-KIND:cols :}
   nd MIR-OP@ {: op:n :}
   op OP-RESHAPE   = if nd RB-VD-RESHAPE     exit then
   op OP-TRANSPOSE = if MV-TRANSPOSE-VERDICT exit then
   op OP-SLICE     = if nd cols RB-VD-SLICE  exit then
   op OP-CONCAT    = if MV-CONCAT-VERDICT    exit then
   op OP-GATHER    = if MV-GATHER-VERDICT    exit then
   E-CAD-BIND-SHAPE throw ;

: REPROP-MOVE ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd RB-MOVE-SHAPE {: rows:CAD-KIND:rows cols:CAD-KIND:cols :}
   nd MIR-ATTR@ {: attr:n :}
   attr MV-TF@  nd cols RB-MOVE-VD  attr MV-PA@  attr MV-PB@  MV-PACK  nd MIR-ATTR!
   rows cols nd MIR-SHAPE! ;

\ param-operand legality re-check over IR operands (mirrors the capture SHP-CHECK):
\ operand 1 must broadcast-match operand 0 under the node op's class. Unary elementwise
\ and row-reduce ops have no param operand (count < 2) and skip.
: RB-EW-PARAM ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd MIR-IN-COUNT@ 2 < if exit then
   nd 0 MIR-INPUT-IDX MIR-IN@ {: d:MIR:operand-ref :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: p:MIR:operand-ref :}
   d RB-REF-ROWS d RB-REF-COLS  p RB-REF-ROWS p RB-REF-COLS  nd MIR-OP@  SHP-CHECK ;

\ contraction re-check: the linear bias (operand 2) must be 1 x (output cols); matmul
\ (no bias operand) skips. Inner-dim agreement stays in RB-MM.
: RB-MM-BIAS ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd MIR-IN-COUNT@ 3 < if exit then
   nd 0 MIR-INPUT-IDX MIR-IN@ {: x:MIR:operand-ref :}
   nd 1 MIR-INPUT-IDX MIR-IN@ {: w:MIR:operand-ref :}
   nd 2 MIR-INPUT-IDX MIR-IN@ {: b:MIR:operand-ref :}
   x RB-REF-ROWS w RB-REF-COLS  b RB-REF-ROWS b RB-REF-COLS  OP-BIAS  SHP-CHECK ;

: REPROP-NODE ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd MIR-OP@ OPR-CLASS {: cls:n :}
   cls CLASS-MOVEMENT = if nd REPROP-MOVE exit then
   cls CLASS-MATMUL   = if nd RB-MM-BIAS nd RB-MM nd MIR-SHAPE! exit then
   nd RB-EW-PARAM  nd RB-DATA nd MIR-SHAPE! ;

: REPROP-ALL ( -- )  MIR-N@ 0 ?do  i MIR-NODE-ID REPROP-NODE  loop ;

\ ---- BIND-SHAPES parse: positional "[name:]RxC" specs, one per input slot ------
64 constant BS-CAP
create BS-ROWS BS-CAP cells allot
create BS-COLS BS-CAP cells allot
variable BS-N

: BS-RESET ( -- )  0 BS-N ! ;
: BS-PUSH ( n n -- ) {: rows:n cols:n :}
   BS-N @ BS-CAP >= if E-CAD-BIND-COUNT throw then
   rows 0 <= cols 0 <= or if E-CAD-BIND-SHAPE throw then     \ a bind spec is concrete
   rows BS-ROWS BS-N @ cells + !
   cols BS-COLS BS-N @ cells + !
   BS-N @ 1+ BS-N ! ;
: BS-SPEC ( ptr u8 n -- )  PARSE-SHAPE BS-PUSH ;
: BS-PARSE ( -- )
   BS-RESET
   begin
      parse-name dup 0= if 2drop E-CAD-SYNTAX throw then
      2dup s" ;" STR= if 2drop exit then
      BS-SPEC
   again ;

\ merge one extent: unbound (0) takes the spec; a bound extent must equal the spec.
: BS-ROW ( CAD-KIND:rows n -- CAD-KIND:rows )
   {: cur:CAD-KIND:rows new:n :}
   cur 0 ROWS-IS? if new 1 SHAPE drop exit then
   cur new ROWS-IS? 0= if E-CAD-BIND-CONFLICT throw then
   cur ;
: BS-COL ( CAD-KIND:cols n -- CAD-KIND:cols )
   {: cur:CAD-KIND:cols new:n :}
   cur 0 COLS-IS? if 1 new SHAPE nip exit then
   cur new COLS-IS? 0= if E-CAD-BIND-CONFLICT throw then
   cur ;
: BS-APPLY ( n -- ) {: i:n :}
   i MIR-SLOT-ID {: s:MIR:input-slot :}
   s MIR-SLOT-ROWS@ BS-ROWS i cells + @ BS-ROW
   s MIR-SLOT-COLS@ BS-COLS i cells + @ BS-COL
   s MIR-SLOT-SHAPE! ;
: BS-BIND ( -- )
   BS-N @ MIR-IN-SLOTS@ <> if E-CAD-BIND-COUNT throw then
   MIR-IN-SLOTS@ 0 ?do  i BS-APPLY  loop
   REPROP-ALL
   FP-RESET ;

public

\ BIND-SHAPES rebinds declared input extents AFTER MODEL:, positional in signature
\ order (PARSE-SIG keeps no names): the i-th "[name:]RxC" spec binds input slot i,
\ filling an unbound (0) MODEL: extent or restating a bound one. A differing bound
\ extent, a wrong spec count, a malformed/zero spec, or an illegal re-propagated
\ downstream shape all fail closed. On success node extents re-propagate over the IR
\ and the fusion plan is dropped (FP-RESET) so FUSE/MEMORY/TILE re-plan.
: BIND-SHAPES ( -- )
   MODEL-SET? @ 0= if E-CAD-NOMODEL throw then
   BS-PARSE  BS-BIND ;

private

\ ---- report builders (read model-IR facts, write the report) ---------------
: CAD-BASE ( report -- report )                \ model name + real node counts
   MODEL-SET? @ 0= if E-CAD-NOMODEL throw then
   MODEL-NAME$ REPORT:MODEL!
   MODEL-K MODEL-K REPORT:OPS!
   MODEL-K REPORT:REGIONS!
   MIR-MAT-COUNT REPORT:MATERIALIZED! ;

: LOWER-KEYS ( report -- report )              \ shape/dtype/layout of the model output
   MIR-N@ 0= if exit then
   MIR-N@ 1- MIR-NODE-ID {: out:CAD-KIND:node-id :}
   out MIR-SHAPE-KEY  REPORT:SHAPE!
   out MIR-DTYPE-KEY  REPORT:DTYPE!
   out MIR-LAYOUT-KEY REPORT:LAYOUT! ;

: LOWER-INTO ( report -- report )
   CAD-BASE
   LOWER-KEYS
   s" lowering: model-IR node table (cad-1)" REPORT:WARN+ ;

\ FUSE plans regions (maki/fusion-plan.f) then estimates traffic (maki/traffic.f):
\ ops before (nodes) / after (regions), the typed split rows, the materialized count
\ from the updated IR flags, and estimated bytes before/after when the shapes bind.
: FUSE-INTO ( report -- report )
   FP-BUILD
   MODEL-K FP-REGION-COUNT REPORT:OPS!
   FP-REGION-COUNT REPORT:REGIONS!
   MIR-MAT-COUNT REPORT:MATERIALIZED!
   FP-REPORT+
   TRF-INTO ;

\ ---- movement materialization rows (MEMORY reads the IR facts) --------------
: MOVE-WARN$ ( CAD-KIND:node-id -- ptr u8 n )
   {: node:CAD-KIND:node-id :}                    \ one movement node's traffic-cost row
   SB-RESET
   s" memory.move: node " SB-APPEND  node NODE>RAW SB-INT
   $20 SB-APPEND-C  node MIR-OP@ OPR-NAME SB-APPEND
   s"  verdict=" SB-APPEND  node MIR-MOVE-VERDICT@ MV-VD-NAME SB-APPEND
   s"  reason="  SB-APPEND  node MIR-OP@ MV-REASON$ SB-APPEND
   SB$ ;

: MEM-MOVE-ROW+ ( report CAD-KIND:node-id -- report ) {: node:CAD-KIND:node-id :}
   node MIR-MOVE? 0= if exit then                          \ compute nodes carry no row
   node MIR-MOVE-VERDICT@ MV-VD-REPORTS? 0= if exit then   \ free/staged: no traffic cost
   node MOVE-WARN$ REPORT:WARN+ ;

: MEM-MOVE-ROWS ( report -- report )
   MIR-N@ 0 ?do  i MIR-NODE-ID MEM-MOVE-ROW+  loop ;

: MEMORY-INTO ( report -- report )
   FP-BUILD                                          \ region + materialization flags (6.x)
   MEM-PLAN-INTO                                     \ per-hot coalescing status + tail/align rows
   MEM-MOVE-ROWS ;                                   \ movement materialization rows (landed)

\ ---- schedule (cad-4): family selection + all candidates + closed-form default -
\ TILE picks the schedule family from region 0's class mix (v1 first-region only),
\ prints every candidate of that family, selects the deterministic default, renders
\ the section 7.4 cache key, and reports the replay miss ("using defaults"), since
\ cad-4 has no measurements (those land in cad-5/cad-6).
: REGION-HAS-SOFTMAX? ( n -- bool ) {: r:n :}   \ region carries a softmax-row op (two reductions)
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node FP-RID@ r =  node MIR-OP@ OP-SOFTMAX-ROW =  and if unloop true exit then
   loop false ;

: REGION-FAM ( n -- n ) {: r:n :}               \ region -> schedule family id
   r FP-REGION-CLASSMIX  r REGION-HAS-SOFTMAX?  FAM-SELECT ;

\ max legal vector width for the region output's compiler-allocated (AL-16) write;
\ this is the elementwise default's "max legal vec" (else scalar for a strided write).
: REGION-MAXVEC ( CAD-KIND:node-id -- n ) {: rep:CAD-KIND:node-id :}
   rep MIR-LAY@ LAY-ROW LAYOUT-EQUAL? 0= if 1 exit then
   AL-16 rep MIR-DT@ DT-SIZE DIM-RAW rep MIR-COLS@ COLS-RAW MP-W ;

: TILE-CANDS+ ( report n -- report ) {: fam:n :}   \ emit every candidate row of a family
   fam FAM-SPACE 0 ?do  fam i CAND$ REPORT:CAND+  loop ;

\ replay lookup is the cad-5 store seam: a miss means the shape class is unmeasured.
: TILE-REPLAY-NOTE ( report n -- report ) {: r:n :}
   r TARGET:SM87 SK-KEY$ SK-GET nip if exit then
   s" schedule: unmeasured shape class -> using defaults" REPORT:WARN+ ;

: TILE-INTO ( report -- report )
   FP-BUILD
   0 REGION-FAM {: fam:n :}
   0 SK-REGION-REP {: rep:CAD-KIND:node-id :}
   fam TILE-CANDS+
   fam rep MIR-COLS@ COLS-RAW rep REGION-MAXVEC FAM-DEFAULT REPORT:SELECT!
   0 TARGET:SM87 SK-KEY$ REPORT:CACHE!
   0 TILE-REPLAY-NOTE
   s" schedule: defaults (unmeasured shape class - cad-6 tunes)" REPORT:WARN+
   s" schedule: family from region 0 only (v1 limitation)" REPORT:WARN+ ;

: TUNE-INTO ( report -- report )
   TILE-INTO
   s" tune: measurement needs device (cad-6)" REPORT:WARN+ ;

: CERTIFY-INTO ( report -- report )            \ static, no GPU: model-level legality
   s" " V-PASS G-CERTIFY REPORT:GATE!
   s" certify: model-level legality only; kernel legality in cad-5" REPORT:WARN+ ;

\ GOLDEN is REAL (maki/golden.f + maki/lower-golden.f). Precedence: an external reference
\ artifact wins; else, when a device is present and the model is device-lowerable, the DEVICE
\ model golden runs the whole forward IR on the GPU (cross-region device buffers) and compares
\ the final output vs the host executor under a composed f32 tolerance (LOWER-MODEL-GOLDEN,
\ installed via golden.f's device hook); else the host self-consistency oracle runs. Off-device
\ the device leg is inert, so the host legs are unchanged. GOLDEN-INTO is provided by maki/golden.f.

\ GRADCHECK is REAL on the host now (maki/gradcheck.f): a numeric model-level
\ gradcheck for reference-complete, host-executable (elementwise) models; models with
\ a reduction/matmul/rope op or a missing adjoint stay honestly not-run (named reason).
\ GRADCHECK-INTO is provided by maki/gradcheck.f.

: PROFILE-INTO ( report -- report )
   s" no-device" V-NOTRUN G-PROFILE REPORT:GATE! ;

\ ---- device golden leg (cad.f owns the device dependency; golden.f stays device-free) ---------
\ GOLDEN precedence: external artifact > DEVICE model golden > host self-consistency. The device
\ leg runs when a GPU is present, every region's cubin is registered (MDL-CUBIN!), and the model is
\ device-lowerable; LOWER-MODEL-GOLDEN executes the whole forward IR on the GPU with cross-region
\ device buffers and compares the final output vs the host executor under the composed tolerance
\ of each region class's ACTIVE precision (maki/precision.f - the licensed-precision rows).
\ Off-device (or without cubins / a non-lowerable model) GOLDEN-GATE-INTO is exactly GOLDEN-INTO,
\ so the host gates are unchanged.
: GOLDEN-GATE-DEVICE ( report -- report )
   LOWER-MODEL-GOLDEN {: v:n :}
   -1 GOLDEN-DEV!                                  \ evidence: the device leg produced this verdict
   LG-PREC-USED@ GOLDEN-PREC!                      \ evidence: the precision it was judged under
   LOWER-GOLDEN-REASON$ v G-GOLDEN REPORT:GATE!
   s" golden: device model golden (cross-region vs host, composed licensed-precision tolerance)" REPORT:WARN+ ;
: GOLDEN-GATE-INTO ( report -- report )
   GA-EXISTS? if GOLDEN-INTO exit then             \ external artifact wins (GOLDEN-INTO selects it)
   CUDA:OPEN? if
      FP-BUILD                                     \ the device legs read the region plan
      MDL-CUBINS-READY? if MDL-LOWERABLE? if GOLDEN-GATE-DEVICE exit then then
   then
   GOLDEN-INTO ;                                   \ host self-consistency (device flag cleared there)

\ full conservative report over every phase (PROMOTE / OPTIMIZE / EXPLAIN)
: FULL-REPORT ( -- report )
   REPORT:NEW LOWER-INTO FUSE-INTO MEMORY-INTO TILE-INTO
   CERTIFY-INTO GOLDEN-GATE-INTO GRADCHECK-INTO PROFILE-INTO ;

\ ---- promotion gate (CAD 7c gate-set alignment) ----------------------------
: GATE-PASS? ( report n -- report bool )
   over swap REPORT:GATE-TAG@ V-PASS = ;
: GATE-NOT-FAIL? ( report n -- report bool )   \ pass or not-run, but not a real fail
   over swap REPORT:GATE-TAG@ V-FAIL <> ;
: GATE-RECORDED? ( report n -- report bool )   \ a recorded verdict (any legal tag)
   over swap REPORT:GATE-TAG@ dup 0 >= swap V-N < and ;

\ PROMOTE gate set (docs/model-cad.md Phase 7 / CAD-PLAN, cad-7 UPDATE fold):
\ a model promotes when CERTIFY passes AND GOLDEN passes AND GRADCHECK did not
\ FAIL. GRADCHECK not-run (the model has no host-differentiable backward - cast /
\ decode) clears the gate exactly like a pass; only a real gradient mismatch
\ (V-FAIL) blocks. PROFILE is mandatory-to-run but NON-blocking: FULL-REPORT
\ always runs PROFILE-INTO so a verdict is recorded, yet its value (not-run
\ off-device, or a device roofline tag on Orin) never gates promotion.
: PROMOTE-OK? ( report -- report bool )
   G-CERTIFY   GATE-PASS?      >r
   G-GOLDEN    GATE-PASS?      r> and >r
   G-GRADCHECK GATE-NOT-FAIL?  r> and >r
   G-PROFILE   GATE-RECORDED?  r> and ;

: CACHE-KEY-INTO ( report -- report )          \ artifact key (model-scoped in phase 1)
   MODEL-NAME$ REPORT:CACHE! ;

: PROMOTE-REPORT ( report -- report )
   PROMOTE-OK? 0= if E-CAD-GATE throw then
   CACHE-KEY-INTO ;

\ On a passing PROMOTE, write the artifact record to the CAD store (maki/store.f):
\ an evidence row (the four gate verdicts) and a schedules row (region-0 selection),
\ both keyed by the section 7.4 key of region 0 (the same key TILE/OPTIMIZE cache). A
\ refused promote throws in PROMOTE-REPORT before this runs, so no partial rows land.
: PROMOTE-EVIDENCE ( report -- report )
   dup G-CERTIFY   REPORT:GATE-TAG@ {: c:n :}
   dup G-GOLDEN    REPORT:GATE-TAG@ {: g:n :}
   dup G-GRADCHECK REPORT:GATE-TAG@ {: gc:n :}
   dup G-PROFILE   REPORT:GATE-TAG@ {: p:n :}
   dup REPORT:SELECT@ {: sel:n :}
   0 TARGET:SM87 SK-KEY$ c g gc p GOLDEN-DEV? GOLDEN-PREC@ EVID-PUT-G  \ golden=device-<v>:<prec> when the device leg ran
   0 TARGET:SM87 SK-KEY$ sel SCHED-PUT ;

: OPTIMIZE-PROMOTE ( report -- report )        \ record the decision, never throw
   PROMOTE-OK? if
      CACHE-KEY-INTO  s" promote: gates pass; artifact cached" REPORT:WARN+
   else
      s" promote: refused; certify/golden/gradcheck gate not satisfied" REPORT:WARN+
   then ;

public

\ ---- inspection commands (each returns a structured cad-0a report) ----------
: LOWER ( -- report )      REPORT:NEW LOWER-INTO ;
: FUSE ( -- report )       REPORT:NEW LOWER-INTO FUSE-INTO ;
: MEMORY ( -- report )     REPORT:NEW LOWER-INTO MEMORY-INTO ;
: TILE ( -- report )       REPORT:NEW LOWER-INTO TILE-INTO ;
: CERTIFY ( -- report )    REPORT:NEW LOWER-INTO CERTIFY-INTO ;
: GOLDEN ( -- report )     REPORT:NEW LOWER-INTO GOLDEN-GATE-INTO ;
: GRADCHECK ( -- report )  REPORT:NEW LOWER-INTO GRADCHECK-INTO ;
: PROFILE ( -- report )    REPORT:NEW LOWER-INTO PROFILE-INTO ;
: TUNE ( -- report )       REPORT:NEW LOWER-INTO TUNE-INTO ;

\ PROMOTE refuses (named throw) unless every gate passes; on success caches the key
\ and writes the artifact + evidence rows to the CAD store.
: PROMOTE ( -- report )  FULL-REPORT PROMOTE-REPORT PROMOTE-EVIDENCE ;

\ OPTIMIZE composes lower -> fuse -> memory -> tile -> gates -> promote decision.
: OPTIMIZE ( -- report )  FULL-REPORT OPTIMIZE-PROMOTE ;

\ EXPLAIN emits repair-packet-discipline failure lines for every non-pass gate.
: EXPLAIN ( -- ptr u8 n )  FULL-REPORT REPORT:RENDER-PACKETS ;

\ CAD-SHOW renders a report's machine view to stdout (interactive convenience).
: CAD-SHOW ( report -- )  REPORT:RENDER type cr ;

;package
