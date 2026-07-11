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
create CAP-INS CAP-CAP cells allot         \ declared input tensor handles (as n), order 0..N-1
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
: OP-LOOKUP ( ptr u8 n -- opkind bool )
   2dup s" ADD"          STR= if 2drop MAKI-OPKIND:ADD          true exit then
   2dup s" MUL"          STR= if 2drop MAKI-OPKIND:MUL          true exit then
   2dup s" SCALE"        STR= if 2drop MAKI-OPKIND:SCALE        true exit then
   2dup s" BIAS"         STR= if 2drop MAKI-OPKIND:BIAS         true exit then
   2dup s" RELU"         STR= if 2drop MAKI-OPKIND:RELU         true exit then
   2dup s" GELU"         STR= if 2drop MAKI-OPKIND:GELU         true exit then
   2dup s" SILU"         STR= if 2drop MAKI-OPKIND:SILU         true exit then
   2dup s" LAYERNORM"    STR= if 2drop MAKI-OPKIND:LAYERNORM    true exit then
   2dup s" RMSNORM"      STR= if 2drop MAKI-OPKIND:RMSNORM      true exit then
   2dup s" SOFTMAX-ROW"  STR= if 2drop MAKI-OPKIND:SOFTMAX-ROW  true exit then
   2dup s" MATMUL"       STR= if 2drop MAKI-OPKIND:MATMUL       true exit then
   2dup s" LINEAR"       STR= if 2drop MAKI-OPKIND:LINEAR       true exit then
   2dup s" RESIDUAL-ADD" STR= if 2drop MAKI-OPKIND:RESIDUAL-ADD true exit then
   2dup s" CAST"         STR= if 2drop MAKI-OPKIND:CAST         true exit then
   2dup s" ROPE"         STR= if 2drop MAKI-OPKIND:ROPE         true exit then
   2dup s" RESHAPE"      STR= if 2drop MAKI-OPKIND:RESHAPE      true exit then
   2dup s" TRANSPOSE"    STR= if 2drop MAKI-OPKIND:TRANSPOSE    true exit then
   2dup s" SLICE"        STR= if 2drop MAKI-OPKIND:SLICE        true exit then
   2dup s" CONCAT"       STR= if 2drop MAKI-OPKIND:CONCAT       true exit then
   2dup s" GATHER"       STR= if 2drop MAKI-OPKIND:GATHER       true exit then
   2drop MAKI-OPKIND:ADD false ;                       \ placeholder value (bool false; callers ignore it)

: OP-KIND ( ptr u8 n -- opkind )
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
: SHP-BOUND? ( n n n n -- bool ) {: dr:n dc:n pr:n pc:n :}   \ all four extents bound (nonzero)
   dr 0<> dc 0<> and  pr 0<> and  pc 0<> and ;
: SHP-SAME? ( n n n n -- bool ) {: dr:n dc:n pr:n pc:n :}    \ param EQUALS data
   pr dr =  pc dc =  and ;
: SHP-ROW? ( n n n -- bool ) {: dc:n pr:n pc:n :}            \ param is 1 x dc
   pr 1 =  pc dc =  and ;
: SHP-SCALAR? ( n n -- bool ) {: pr:n pc:n :}                \ param is 1 x 1
   pr 1 =  pc 1 =  and ;
\ per-class broadcast legality; an unbound (0) extent defers to BIND-SHAPES reprop
: SHP-ROW-OK? ( n n n n -- bool ) {: dr:n dc:n pr:n pc:n :}        \ BIAS: param 1 x dc
   dr dc pr pc SHP-BOUND? 0= if true exit then  dc pr pc SHP-ROW? ;
: SHP-SAME-OK? ( n n n n -- bool ) {: dr:n dc:n pr:n pc:n :}       \ ADD/MUL/RESIDUAL: param == data
   dr dc pr pc SHP-BOUND? 0= if true exit then  dr dc pr pc SHP-SAME? ;
: SHP-SCALE-OK? ( n n n n -- bool ) {: dr:n dc:n pr:n pc:n :}      \ SCALE: param == data OR 1 x 1
   dr dc pr pc SHP-BOUND? 0= if true exit then  dr dc pr pc SHP-SAME?  pr pc SHP-SCALAR? or ;

\ param-operand broadcast legality dispatched on the op family (exhaustive MATCH:
\ adding an op forces a broadcast-class decision here). BIAS/SCALE/ADD/MUL/RESIDUAL
\ carry a documented class; every other op is unconstrained (any param shape legal).
: SHP-LEGAL? ( n n n n opkind -- bool )
   MATCH opkind
      bias            OF SHP-ROW-OK?   ENDOF
      scale           OF SHP-SCALE-OK? ENDOF
      add             OF SHP-SAME-OK?  ENDOF
      mul             OF SHP-SAME-OK?  ENDOF
      residual-add    OF SHP-SAME-OK?  ENDOF
      relu            OF 2drop 2drop true ENDOF
      gelu            OF 2drop 2drop true ENDOF
      layernorm       OF 2drop 2drop true ENDOF
      rmsnorm         OF 2drop 2drop true ENDOF
      softmax-row     OF 2drop 2drop true ENDOF
      matmul          OF 2drop 2drop true ENDOF
      linear          OF 2drop 2drop true ENDOF
      cast            OF 2drop 2drop true ENDOF
      silu            OF 2drop 2drop true ENDOF
      rope            OF 2drop 2drop true ENDOF
      reshape         OF 2drop 2drop true ENDOF
      transpose       OF 2drop 2drop true ENDOF
      slice           OF 2drop 2drop true ENDOF
      concat          OF 2drop 2drop true ENDOF
      gather          OF 2drop 2drop true ENDOF
      relu-bwd        OF 2drop 2drop true ENDOF
      gelu-bwd        OF 2drop 2drop true ENDOF
      silu-bwd        OF 2drop 2drop true ENDOF
      layernorm-bwd   OF 2drop 2drop true ENDOF
      rmsnorm-bwd     OF 2drop 2drop true ENDOF
      softmax-row-bwd OF 2drop 2drop true ENDOF
      rope-bwd        OF 2drop 2drop true ENDOF
      rowsum-bwd      OF 2drop 2drop true ENDOF
      fullsum-dot-bwd OF 2drop 2drop true ENDOF
      pad-scatter     OF 2drop 2drop true ENDOF
      scatter-add     OF 2drop 2drop true ENDOF
      gelu-bwd2       OF 2drop 2drop true ENDOF
   ;MATCH ;
: SHP-CHECK ( n n n n opkind -- )
   SHP-LEGAL? 0= if E-CAD-PARAM-SHAPE throw then ;

\ capture entry points: elementwise param vs its data operand; linear bias vs output cols.
\ EW-SHAPE-CHECK's op family rides the return stack while the extents compute, so it
\ lands on top for SHP-CHECK (an opkind cannot bind into a local).
: EW-SHAPE-CHECK ( tensor tensor opkind -- ) >r {: x:tensor p:tensor :}
   x TENSOR:TV-ROWS@ x TENSOR:TV-COLS@  p TENSOR:TV-ROWS@ p TENSOR:TV-COLS@  r> SHP-CHECK ;
: LIN-BIAS-CHECK ( tensor tensor tensor -- ) {: x:tensor w:tensor b:tensor :}
   x TENSOR:TV-ROWS@ w TENSOR:TV-COLS@  b TENSOR:TV-ROWS@ b TENSOR:TV-COLS@  MAKI-OPKIND:BIAS  SHP-CHECK ;

\ ---- post-capture param-operand shape legality (over the captured plan store) --------
\ The named vocabulary is the arity/kind surface only (plan-vocab.f); broadcast legality
\ layers here, exactly as the old per-op capture did. After the compiled body runs and
\ populates the plan store, each elementwise/linear node's parameter operand is re-checked
\ against its data operand's shape. An unbound (0) extent defers to BIND-SHAPES reprop.
: PLAN-SHP-NODE ( n -- ) {: j:n :}
   j TENSOR:PLAN-OP@ OPR-CLASS {: cls:n :}   \ op is a family; refetch it for the check below
   cls CLASS-EW = if
      j TENSOR:PLAN-IN-COUNT@ 2 < if exit then                       \ unary elementwise: no param operand
      j 0 TENSOR:PLAN-IN@  j 1 TENSOR:PLAN-IN@  j TENSOR:PLAN-OP@ EW-SHAPE-CHECK  exit then
   cls CLASS-MATMUL = if
      j TENSOR:PLAN-IN-COUNT@ 3 < if exit then                       \ matmul (no bias operand): skip
      j 0 TENSOR:PLAN-IN@  j 1 TENSOR:PLAN-IN@  j 2 TENSOR:PLAN-IN@  LIN-BIAS-CHECK then ;
: PLAN-SHP-ALL ( -- )  TENSOR:PLAN-N@ 0 ?do  i PLAN-SHP-NODE  loop ;

\ ---- bridge the captured plan into the model-IR node table -----------------
: PLAN-REF ( tensor -- n )                      \ plan tensor handle -> MIR operand ref
   TENSOR:tensor>N {: h:n :}
   h CAP-IN-N @ < if h MIR-IN-REF else h CAP-IN-N @ - then ;

\ movement nodes materialize only on a materialize/gathered verdict; compute nodes
\ stay materialized (the conservative cad-1 default until the fusion planner lands).
: BRIDGE-MAT ( opkind n -- n ) {: attr:n :}     \ op-kind attr -> materialization flag
   OPR-CLASS CLASS-MOVEMENT = if
      attr MV-VD@ MV-VD-REPORTS? if 1 else 0 then
   else 1 then ;

: BRIDGE-NODE ( n -- ) {: j:n :}
   j TENSOR:PLAN-OP@ MIR-OP-BEGIN               \ op family straight from the plan store
   j TENSOR:PLAN-IN-COUNT@ 0 ?do  j i TENSOR:PLAN-IN@ PLAN-REF MIR-IN+  loop
   j TENSOR:PLAN-OUT@ {: y:tensor :}
   j TENSOR:PLAN-ATTR@ {: attr:n :}
   y TENSOR:TV-ROWS@ y TENSOR:TV-COLS@ y TENSOR:TV-DTYPE@ y TENSOR:TV-LAYOUT@  attr  j TENSOR:PLAN-OP@ attr BRIDGE-MAT  MIR-OP+ drop ;

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
   STR>NUMBER? STR>OPTION
   MATCH option none OF E-CAD-SYNTAX throw ENDOF some OF ENDOF ;MATCH ;

: PARSE-SHAPE ( ptr u8 n -- n n ) {: a:ptr u:n :}   \ "name:RxC" or "RxC" -> rows cols
   a u $3A INDEX-OF MATCH option                 \ shape span starts past any "name:"
     none OF 0 ENDOF
     some OF IDX>N 1+ ENDOF
   ;MATCH {: off:n :}
   a off +  u off -  $78 INDEX-OF MATCH option   \ 'x' index within the shape span
     none OF E-CAD-SYNTAX throw ENDOF
     some OF IDX>N ENDOF
   ;MATCH {: xi:n :}
   a off +          xi           PARSE-INT       \ rows
   a off + xi 1+ +  u off - xi 1+ -  PARSE-INT ; \ cols

: SKIP-TO-RPAREN ( -- )                         \ swallow output names up to ')'
   begin
      parse-name dup 0= if 2drop E-CAD-SYNTAX throw then
      s" )" STR= if exit then
   again ;

\ the name span of a "name:RxC" spec (empty when the spec is a bare "RxC")
: SPEC-NAME ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u $3A INDEX-OF MATCH option
     none OF a 0 ENDOF
     some OF IDX>N {: ci:n :} a ci ENDOF
   ;MATCH ;

\ one "[name:]RxC" spec: build the input descriptor + IR slot, and intern its local name
\ (synthesized when the spec is a bare "RxC") into NT slot == input index.
: CAP-SYNTH-NAME ( -- )                             \ bare RxC input: bind IN<idx>
   SB-RESET  s" IN" SB-APPEND  CAP-IN-N @ SB-INT  SB$ NT-BIND drop ;
: SIG-INPUT ( ptr u8 n -- ) {: a:ptr u:n :}
   CAP-IN-N @ CAP-CAP >= if E-CAD-INPUTS throw then
   a u PARSE-SHAPE {: rows:n cols:n :}
   rows cols MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW TENSOR:TV-DESC TENSOR:tensor>N  CAP-INS CAP-IN-N @ cells + !   \ handle for the seed
   rows cols MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop                 \ register the IR input slot
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
   a u $2E INDEX-OF MATCH option                     \ first '.'
     none OF E-CAD-SYNTAX throw ENDOF
     some OF IDX>N ENDOF
   ;MATCH {: di:n :}
   di 1+ u >= if E-CAD-SYNTAX throw then
   a di 1+ + c@ $2E <> if E-CAD-SYNTAX throw then     \ require the second '.'
   a di            PARSE-INT                          \ r0
   a di 2 + +  u di 2 + -  PARSE-INT ;                \ r1

\ "MOVE:params" body token (RESHAPE:RxC | SLICE:R0..R1): emit the scalar params then the word.
\ The scalar params are the vocab word's ( tensor n n -- tensor ) integer operands.
: EMIT-MOVE-PARAM ( ptr u8 n ptr u8 n -- ) {: op:ptr opu:n pa:ptr pu:n :}
   op opu OP-KIND                                    \ ( mop )  op family cannot bind into a local
   dup MAKI-OPKIND:RESHAPE MAKI-OPKIND:EQ if  drop  pa pu PARSE-SHAPE  swap MSRC-INT MSRC-SP MSRC-INT MSRC-SP
      op opu CAP-EMIT-OP  exit  then
   MAKI-OPKIND:SLICE MAKI-OPKIND:EQ if  pa pu PARSE-RANGE  swap MSRC-INT MSRC-SP MSRC-INT MSRC-SP
      op opu CAP-EMIT-OP  exit  then
   E-CAD-SYNTAX throw ;                              \ only reshape/slice carry ':' params

\ one body op token (not a ">V" and not a bare reference): translate to explicit stack form.
: EMIT-OP-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   a u $3A INDEX-OF MATCH option
     none OF
      a u OP-KIND                                                  \ ( op )  family stays on the stack
      dup MAKI-OPKIND:RESHAPE MAKI-OPKIND:EQ over MAKI-OPKIND:SLICE MAKI-OPKIND:EQ or if E-CAD-SYNTAX throw then \ reshape/slice need ":params"
      OPR-ARITY 1- CAP-EMIT-PARAMS                                 \ tensor params = arity-1
      a u CAP-EMIT-OP  exit
     ENDOF
     some OF IDX>N ENDOF
   ;MATCH {: ci:n :}
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
   CAP-IN-N @ 0 ?do  i cells CAP-INS + @  loop     \ push all N input descriptor handles
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
: RB-REF-ROWS ( n -- n ) {: r:n :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-ROWS@ else r MIR-ROWS@ then ;
: RB-REF-COLS ( n -- n ) {: r:n :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-COLS@ else r MIR-COLS@ then ;
: RB-REF-LAY ( n -- layout ) {: r:n :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-LAY@ else r MIR-LAY@ then ;

\ elementwise / row-reduce forward: output = data operand (operand 0) shape
: RB-DATA ( n -- n n ) {: nd:n :}  nd 0 MIR-IN@ {: r:n :}  r RB-REF-ROWS  r RB-REF-COLS ;

\ contraction: rows from data operand, cols from weight; inner dim must agree
: RB-MM ( n -- n n ) {: nd:n :}
   nd 0 MIR-IN@ {: xr:n :}  nd 1 MIR-IN@ {: wr:n :}
   xr RB-REF-COLS wr RB-REF-ROWS <> if E-CAD-BIND-SHAPE throw then
   xr RB-REF-ROWS  wr RB-REF-COLS ;

\ movement extents per op (attrs carry reshape target / slice range; the rest come
\ from operand extents). Each binds its locals at entry (no branch-local rebinds).
: RB-RESHAPE ( n -- n n ) {: nd:n :}
   nd MIR-ATTR@ {: attr:n :}  nd 0 MIR-IN@ {: r0:n :}
   attr MV-PA@ {: tr:n :}  attr MV-PB@ {: tc:n :}
   r0 RB-REF-ROWS r0 RB-REF-COLS *  tr tc *  <> if E-CAD-BIND-SHAPE throw then
   tr tc ;
: RB-TRANSPOSE ( n -- n n ) {: nd:n :}
   nd 0 MIR-IN@ {: r0:n :}  r0 RB-REF-COLS  r0 RB-REF-ROWS ;
: RB-SLICE ( n -- n n ) {: nd:n :}
   nd MIR-ATTR@ {: attr:n :}  nd 0 MIR-IN@ {: r0:n :}
   attr MV-PA@ {: a:n :}  attr MV-PB@ {: b:n :}
   a 0 < b r0 RB-REF-ROWS > or  a b > or if E-CAD-BIND-SHAPE throw then
   b a -  r0 RB-REF-COLS ;
: RB-CONCAT ( n -- n n ) {: nd:n :}
   nd 0 MIR-IN@ {: r0:n :}  nd 1 MIR-IN@ {: r1:n :}
   r0 RB-REF-COLS r1 RB-REF-COLS <> if E-CAD-BIND-SHAPE throw then
   r0 RB-REF-ROWS r1 RB-REF-ROWS +  r0 RB-REF-COLS ;
: RB-GATHER ( n -- n n ) {: nd:n :}
   nd 0 MIR-IN@ {: r0:n :}  nd 1 MIR-IN@ {: r1:n :}
   r1 RB-REF-ROWS r1 RB-REF-COLS *  r0 RB-REF-COLS ;

: RB-MOVE-SHAPE ( n -- n n ) {: nd:n :}
   nd MIR-OP@ MATCH opkind
      reshape         OF nd RB-RESHAPE   ENDOF
      transpose       OF nd RB-TRANSPOSE ENDOF
      slice           OF nd RB-SLICE     ENDOF
      concat          OF nd RB-CONCAT    ENDOF
      gather          OF nd RB-GATHER    ENDOF
      add             OF E-CAD-BIND-SHAPE throw ENDOF
      mul             OF E-CAD-BIND-SHAPE throw ENDOF
      scale           OF E-CAD-BIND-SHAPE throw ENDOF
      bias            OF E-CAD-BIND-SHAPE throw ENDOF
      relu            OF E-CAD-BIND-SHAPE throw ENDOF
      gelu            OF E-CAD-BIND-SHAPE throw ENDOF
      layernorm       OF E-CAD-BIND-SHAPE throw ENDOF
      rmsnorm         OF E-CAD-BIND-SHAPE throw ENDOF
      softmax-row     OF E-CAD-BIND-SHAPE throw ENDOF
      matmul          OF E-CAD-BIND-SHAPE throw ENDOF
      linear          OF E-CAD-BIND-SHAPE throw ENDOF
      residual-add    OF E-CAD-BIND-SHAPE throw ENDOF
      cast            OF E-CAD-BIND-SHAPE throw ENDOF
      silu            OF E-CAD-BIND-SHAPE throw ENDOF
      rope            OF E-CAD-BIND-SHAPE throw ENDOF
      relu-bwd        OF E-CAD-BIND-SHAPE throw ENDOF
      gelu-bwd        OF E-CAD-BIND-SHAPE throw ENDOF
      silu-bwd        OF E-CAD-BIND-SHAPE throw ENDOF
      layernorm-bwd   OF E-CAD-BIND-SHAPE throw ENDOF
      rmsnorm-bwd     OF E-CAD-BIND-SHAPE throw ENDOF
      softmax-row-bwd OF E-CAD-BIND-SHAPE throw ENDOF
      rope-bwd        OF E-CAD-BIND-SHAPE throw ENDOF
      rowsum-bwd      OF E-CAD-BIND-SHAPE throw ENDOF
      fullsum-dot-bwd OF E-CAD-BIND-SHAPE throw ENDOF
      pad-scatter     OF E-CAD-BIND-SHAPE throw ENDOF
      scatter-add     OF E-CAD-BIND-SHAPE throw ENDOF
      gelu-bwd2       OF E-CAD-BIND-SHAPE throw ENDOF
   ;MATCH ;

\ movement dissolution verdict re-derived from the new extents (slice re-checks its
\ offset/col alignment; the rest are layout- or constant-determined).
: RB-VD-RESHAPE ( n -- n ) {: nd:n :}  nd 0 MIR-IN@ RB-REF-LAY MV-RESHAPE-VERDICT ;
: RB-VD-SLICE ( n n -- n ) {: nd:n cols:n :}
   nd 0 MIR-IN@ RB-REF-LAY  nd MIR-ATTR@ MV-PA@  cols  MV-SLICE-VERDICT ;
: RB-MOVE-VD ( n n -- n ) {: nd:n cols:n :}
   nd MIR-OP@ MATCH opkind
      reshape         OF nd RB-VD-RESHAPE     ENDOF
      transpose       OF MV-TRANSPOSE-VERDICT ENDOF
      slice           OF nd cols RB-VD-SLICE  ENDOF
      concat          OF MV-CONCAT-VERDICT    ENDOF
      gather          OF MV-GATHER-VERDICT    ENDOF
      add             OF E-CAD-BIND-SHAPE throw ENDOF
      mul             OF E-CAD-BIND-SHAPE throw ENDOF
      scale           OF E-CAD-BIND-SHAPE throw ENDOF
      bias            OF E-CAD-BIND-SHAPE throw ENDOF
      relu            OF E-CAD-BIND-SHAPE throw ENDOF
      gelu            OF E-CAD-BIND-SHAPE throw ENDOF
      layernorm       OF E-CAD-BIND-SHAPE throw ENDOF
      rmsnorm         OF E-CAD-BIND-SHAPE throw ENDOF
      softmax-row     OF E-CAD-BIND-SHAPE throw ENDOF
      matmul          OF E-CAD-BIND-SHAPE throw ENDOF
      linear          OF E-CAD-BIND-SHAPE throw ENDOF
      residual-add    OF E-CAD-BIND-SHAPE throw ENDOF
      cast            OF E-CAD-BIND-SHAPE throw ENDOF
      silu            OF E-CAD-BIND-SHAPE throw ENDOF
      rope            OF E-CAD-BIND-SHAPE throw ENDOF
      relu-bwd        OF E-CAD-BIND-SHAPE throw ENDOF
      gelu-bwd        OF E-CAD-BIND-SHAPE throw ENDOF
      silu-bwd        OF E-CAD-BIND-SHAPE throw ENDOF
      layernorm-bwd   OF E-CAD-BIND-SHAPE throw ENDOF
      rmsnorm-bwd     OF E-CAD-BIND-SHAPE throw ENDOF
      softmax-row-bwd OF E-CAD-BIND-SHAPE throw ENDOF
      rope-bwd        OF E-CAD-BIND-SHAPE throw ENDOF
      rowsum-bwd      OF E-CAD-BIND-SHAPE throw ENDOF
      fullsum-dot-bwd OF E-CAD-BIND-SHAPE throw ENDOF
      pad-scatter     OF E-CAD-BIND-SHAPE throw ENDOF
      scatter-add     OF E-CAD-BIND-SHAPE throw ENDOF
      gelu-bwd2       OF E-CAD-BIND-SHAPE throw ENDOF
   ;MATCH ;

: REPROP-MOVE ( n -- ) {: nd:n :}
   nd RB-MOVE-SHAPE {: rows:n cols:n :}
   nd MIR-ATTR@ {: attr:n :}
   attr MV-TF@  nd cols RB-MOVE-VD  attr MV-PA@  attr MV-PB@  MV-PACK  nd MIR-ATTR!
   rows cols nd MIR-SHAPE! ;

\ param-operand legality re-check over IR operands (mirrors the capture SHP-CHECK):
\ operand 1 must broadcast-match operand 0 under the node op's class. Unary elementwise
\ and row-reduce ops have no param operand (count < 2) and skip.
: RB-EW-PARAM ( n -- ) {: nd:n :}
   nd MIR-IN-COUNT@ 2 < if exit then
   nd 0 MIR-IN@ {: d:n :}  nd 1 MIR-IN@ {: p:n :}
   d RB-REF-ROWS d RB-REF-COLS  p RB-REF-ROWS p RB-REF-COLS  nd MIR-OP@  SHP-CHECK ;

\ contraction re-check: the linear bias (operand 2) must be 1 x (output cols); matmul
\ (no bias operand) skips. Inner-dim agreement stays in RB-MM.
: RB-MM-BIAS ( n -- ) {: nd:n :}
   nd MIR-IN-COUNT@ 3 < if exit then
   nd 0 MIR-IN@ {: x:n :}  nd 1 MIR-IN@ {: w:n :}  nd 2 MIR-IN@ {: b:n :}
   x RB-REF-ROWS w RB-REF-COLS  b RB-REF-ROWS b RB-REF-COLS  MAKI-OPKIND:BIAS  SHP-CHECK ;

: REPROP-NODE ( n -- ) {: nd:n :}
   nd MIR-OP@ OPR-CLASS {: cls:n :}
   cls CLASS-MOVEMENT = if nd REPROP-MOVE exit then
   cls CLASS-MATMUL   = if nd RB-MM-BIAS nd RB-MM nd MIR-SHAPE! exit then
   nd RB-EW-PARAM  nd RB-DATA nd MIR-SHAPE! ;

: REPROP-ALL ( -- )  MIR-N@ 0 ?do  i REPROP-NODE  loop ;

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
: BS-DIM ( n n -- n ) {: cur:n new:n :}
   cur 0= if new exit then
   cur new <> if E-CAD-BIND-CONFLICT throw then
   cur ;
: BS-APPLY ( n -- ) {: s:n :}
   s MIR-SLOT-ROWS@  BS-ROWS s cells + @  BS-DIM
   s MIR-SLOT-COLS@  BS-COLS s cells + @  BS-DIM
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
   MIR-N@ 1- {: out:n :}
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
: MOVE-WARN$ ( n -- ptr u8 n ) {: node:n :}     \ one movement node's traffic-cost row
   SB-RESET
   s" memory.move: node " SB-APPEND  node SB-INT
   $20 SB-APPEND-C  node MIR-OP@ OPR-NAME SB-APPEND
   s"  verdict=" SB-APPEND  node MIR-MOVE-VERDICT@ MV-VD-NAME SB-APPEND
   s"  reason="  SB-APPEND  node MIR-OP@ MV-REASON$ SB-APPEND
   SB$ ;

: MEM-MOVE-ROW+ ( report n -- report ) {: node:n :}
   node MIR-MOVE? 0= if exit then                          \ compute nodes carry no row
   node MIR-MOVE-VERDICT@ MV-VD-REPORTS? 0= if exit then   \ free/staged: no traffic cost
   node MOVE-WARN$ REPORT:WARN+ ;

: MEM-MOVE-ROWS ( report -- report )
   MIR-N@ 0 ?do  i MEM-MOVE-ROW+  loop ;

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
      i FP-RID@ r =  i MIR-OP@ MAKI-OPKIND:SOFTMAX-ROW MAKI-OPKIND:EQ  and if unloop true exit then
   loop false ;

: REGION-FAM ( n -- n ) {: r:n :}               \ region -> schedule family id
   r FP-REGION-CLASSMIX  r REGION-HAS-SOFTMAX?  FAM-SELECT ;

\ max legal vector width for the region output's compiler-allocated (AL-16) write;
\ this is the elementwise default's "max legal vec" (else scalar for a strided write).
: REGION-MAXVEC ( n -- n ) {: rep:n :}
   rep MIR-LAY@ LAYOUT-ROW? 0= if 1 exit then
   MAKI-ALIGN:A16  rep MIR-DT@ DT-SIZE  rep MIR-COLS@  MP-W ;

: TILE-CANDS+ ( report n -- report ) {: fam:n :}   \ emit every candidate row of a family
   fam FAM-SPACE 0 ?do  fam i CAND$ REPORT:CAND+  loop ;

\ replay lookup is the cad-5 store seam: a miss means the shape class is unmeasured.
: TILE-REPLAY-NOTE ( report n -- report ) {: r:n :}
   r SK-KEY$ SK-GET nip if exit then
   s" schedule: unmeasured shape class -> using defaults" REPORT:WARN+ ;

: TILE-INTO ( report -- report )
   FP-BUILD
   0 REGION-FAM {: fam:n :}
   0 SK-REGION-REP {: rep:n :}
   fam TILE-CANDS+
   fam  rep MIR-COLS@  rep REGION-MAXVEC  FAM-DEFAULT  REPORT:SELECT!
   0 SK-KEY$ REPORT:CACHE!
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
   0 SK-KEY$ c g gc p GOLDEN-DEV? GOLDEN-PREC@ EVID-PUT-G  \ golden=device-<v>:<prec> when the device leg ran
   0 SK-KEY$ sel SCHED-PUT ;

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

end-package
