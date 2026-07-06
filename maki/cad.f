\ maki/cad.f - Model CAD REPL commands + checked MODEL: capture (dot cad-1).
\
\ MODEL: no longer parses op TOKENS against an inert table (the cad-0b phase-0
\ design). It now CAPTURES by running the model body against the descriptor-mode
\ planning vocabulary (maki/plan-ops.f: PLAN-UNARY / PLAN-BIN-EW / PLAN-MATMUL /
\ PLAN-LINEAR / PLAN-TERN-EW over maki/tensor-value.f descriptors), so every op is
\ a checked `tensor`-typed word, and the captured plan is bridged into the model-IR
\ node table (maki/model-ir.f). The op registry (maki/op-registry.f) supplies each
\ op's arity and class, so capture pulls the right operands and dispatches with no
\ per-op branches beyond arity/class.
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
\ consumers. This named-value capture is the v1 seam for the full CAD-PLAN section 3
\ vision (compile the whole body as one checker-verified composition over descriptors);
\ until that lands, capture runs the body through the checked planning vocabulary and
\ the name table resolves references at capture time.
\
\ Fail closed: unknown op / unbound reference token -> E-CAD-OP; empty body ->
\ E-CAD-EMPTY; malformed signature/shape -> E-CAD-SYNTAX; an op with no data or too
\ few declared inputs -> E-CAD-ARITY; too many inputs -> E-CAD-INPUTS; a bad named
\ value (duplicate, op-shadow, oversized, table full) -> E-CAD-NAME; ">V" with no
\ current value -> E-CAD-NOVALUE; a reference the following op cannot accept ->
\ E-CAD-REF; a param operand whose shape is illegal for the op's broadcast class
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
-5025 constant E-CAD-ARITY     \ op missing its data input or declared parameters
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
create CAP-INS CAP-CAP cells allot         \ declared input tensor handles (as n)
variable CAP-IN-N
variable CAP-CUR                           \ running "current" tensor handle (n; -1 = none)
variable CAP-IP                            \ next unconsumed parameter-input index

\ named value table: name -> value handle (signature input names + ">V" intermediates)
32 constant NT-CAP                         \ max named values per model
16 constant NT-NAME-CAP                    \ max bytes per name
create NT-NAMES NT-CAP NT-NAME-CAP * allot \ fixed-width name text slots
create NT-LENS  NT-CAP cells allot         \ name lengths
create NT-VALS  NT-CAP cells allot         \ value handles (tensor as n)
variable NT-N

\ pending named-operand queue: refs drained (FIFO) by the next op's parameter slots
4 constant CAP-PEND-CAP                    \ max pending named operands before one op
create CAP-PEND CAP-PEND-CAP cells allot   \ pending operand handles (as n)
variable CAP-PEND-N                        \ tail (push index)
variable CAP-PEND-HD                       \ head (dequeue index)

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

\ ---- pending named-operand queue (FIFO; the next op drains it into its params) --
: CAP-PEND-RESET ( -- )  0 CAP-PEND-N !  0 CAP-PEND-HD ! ;
: CAP-PEND-CNT ( -- n )  CAP-PEND-N @ CAP-PEND-HD @ - ;      \ remaining pending refs
: CAP-PEND-PUSH ( n -- )
   CAP-PEND-N @ CAP-PEND-CAP >= if E-CAD-REF throw then
   CAP-PEND-N @ cells CAP-PEND + !  CAP-PEND-N @ 1+ CAP-PEND-N ! ;
: CAP-PEND-DEQ ( -- n )
   CAP-PEND-HD @ cells CAP-PEND + @  CAP-PEND-HD @ 1+ CAP-PEND-HD ! ;

\ ---- named value table (name -> value handle) ------------------------------
: NT-RESET ( -- )  0 NT-N ! ;
: NT-SLOT ( n -- ptr u8 )  NT-NAME-CAP *  NT-NAMES + ;
: NT-FIND ( ptr u8 n -- n bool ) {: a:ptr u:n :}   \ handle valid only when true
   NT-N @ 0 ?do
      a u  i NT-SLOT  i cells NT-LENS + @  STR= if
         i cells NT-VALS + @  true  unloop exit
      then
   loop  0 false ;
: NT-BIND ( ptr u8 n n -- ) {: a:ptr u:n h:n :}    \ bind name -> value handle
   u NT-NAME-CAP > if E-CAD-NAME throw then                 \ name too long
   a u OP-LOOKUP nip if E-CAD-NAME throw then               \ a name may not shadow an op token
   a u NT-FIND   nip if E-CAD-NAME throw then               \ no duplicate name
   NT-N @ NT-CAP >= if E-CAD-NAME throw then                \ table full
   NT-N @ {: i:n :}
   a  i NT-SLOT  u  BYTE-COPY
   u  i cells NT-LENS + !
   h  i cells NT-VALS + !
   i 1+ NT-N ! ;
: NT-BIND-CUR ( ptr u8 n -- ) {: a:ptr u:n :}      \ ">V": name the current value
   CAP-CUR @ 0< if E-CAD-NOVALUE throw then
   CAP-PEND-CNT 0 > if E-CAD-REF throw then                  \ a ref must be consumed before naming
   a u CAP-CUR @ NT-BIND ;

\ ---- capture engine --------------------------------------------------------
: CAP-IN@   ( n -- tensor )  cells CAP-INS + @ >tensor ;
: CAP-CUR@  ( -- tensor )    CAP-CUR @ >tensor ;
: CAP-CUR!  ( tensor -- )    tensor>N CAP-CUR ! ;

: CAP-BEGIN ( -- )
   TV-RESET  PLAN-RESET  MIR-RESET
   0 CAP-IN-N !  1 CAP-IP !  -1 CAP-CUR !
   NT-RESET  CAP-PEND-RESET
   0 MODEL-SET? ! ;

: CAP-INPUT ( n n -- ) {: rows:n cols:n :}      \ declare one model input (f32/row)
   CAP-IN-N @ CAP-CAP >= if E-CAD-INPUTS throw then
   rows cols DT-F32 LAY-ROW TV-DESC {: t:tensor :}
   rows cols DT-F32 LAY-ROW MIR-INPUT+ drop
   t tensor>N  CAP-INS CAP-IN-N @ cells + !
   CAP-IN-N @ 0= if t CAP-CUR! then               \ first input is the running value
   CAP-IN-N @ 1+ CAP-IN-N ! ;

\ declare one model input and (when named) bind its name to the input's value handle
: CAP-INPUT-NAMED ( ptr u8 n n n -- ) {: a:ptr u:n rows:n cols:n :}
   rows cols CAP-INPUT
   u 0 > if  a u  CAP-IN-N @ 1- cells CAP-INS + @  NT-BIND  then ;

: CAP-NEED ( n -- ) {: params:n :}              \ data + params must be available
   CAP-CUR @ 0< if E-CAD-ARITY throw then
   CAP-PEND-CNT params > if E-CAD-REF throw then            \ more refs than the op accepts
   params CAP-PEND-CNT - {: decl:n :}                       \ declared inputs still needed
   CAP-IP @ decl + CAP-IN-N @ > if E-CAD-ARITY throw then ;

\ next op parameter: a pending named ref (FIFO), else the next declared input (advances)
: CAP-PARAM ( -- tensor )
   CAP-PEND-CNT 0 > if CAP-PEND-DEQ >tensor
   else CAP-IP @ CAP-IN@  CAP-IP @ 1+ CAP-IP !  then ;

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
: SHP-LEGAL? ( n n n n n -- bool ) {: dr:n dc:n pr:n pc:n op:n :}
   dr dc pr pc SHP-BOUND? 0= if true exit then             \ unbound -> defer to reprop
   op OP-BIAS = if dc pr pc SHP-ROW? exit then
   op OP-SCALE = if dr dc pr pc SHP-SAME?  pr pc SHP-SCALAR? or exit then
   op OP-ADD = op OP-MUL = or op OP-RESIDUAL-ADD = or if
      dr dc pr pc SHP-SAME? exit then
   true ;                                                  \ op has no documented class
: SHP-CHECK ( n n n n n -- )
   SHP-LEGAL? 0= if E-CAD-PARAM-SHAPE throw then ;

\ capture entry points: elementwise param vs its data operand; linear bias vs output cols
: EW-SHAPE-CHECK ( tensor tensor n -- ) {: x:tensor p:tensor op:n :}
   x TV-ROWS@ x TV-COLS@  p TV-ROWS@ p TV-COLS@  op  SHP-CHECK ;
: LIN-BIAS-CHECK ( tensor tensor tensor -- ) {: x:tensor w:tensor b:tensor :}
   x TV-ROWS@ w TV-COLS@  b TV-ROWS@ b TV-COLS@  OP-BIAS  SHP-CHECK ;

: CAP-OP ( n -- ) {: op:n :}                    \ apply one op-kind to the current value
   op OPR-ARITY {: ar:n :}
   ar 1- CAP-NEED
   op OPR-CLASS CLASS-MATMUL = {: mm:bool :}
   ar 1 = if
      CAP-CUR@ op PLAN-UNARY CAP-CUR!
   else ar 2 = if
      CAP-CUR@ {: x:tensor :}  CAP-PARAM {: p:tensor :}
      mm if x p op PLAN-MATMUL
      else x p op EW-SHAPE-CHECK  x p op PLAN-BIN-EW then CAP-CUR!
   else ar 3 = if
      CAP-CUR@ {: x3:tensor :}  CAP-PARAM {: p1:tensor :}  CAP-PARAM {: p2:tensor :}
      mm if x3 p1 p2 LIN-BIAS-CHECK  x3 p1 p2 op PLAN-LINEAR
      else x3 p1 p2 op PLAN-TERN-EW then CAP-CUR!
   else
      E-CAD-ARITY throw
   then then then
   CAP-PEND-RESET ;

\ ---- movement capture (layout rewrites; scalar params come from the token) ---
\ arity-1 rewrites consume only the running value; concat/gather also consume one
\ declared input as their second operand (like other binary ops).
: CAP-TRANSPOSE ( -- )
   0 CAP-NEED  CAP-CUR@ PLAN-TRANSPOSE CAP-CUR! ;

: CAP-RESHAPE ( n n -- ) {: tr:n tc:n :}
   0 CAP-NEED  CAP-CUR@ tr tc PLAN-RESHAPE CAP-CUR! ;

: CAP-SLICE ( n n -- ) {: r0:n r1:n :}
   0 CAP-NEED  CAP-CUR@ r0 r1 PLAN-SLICE CAP-CUR! ;

: CAP-CONCAT ( -- )
   1 CAP-NEED  CAP-CUR@ CAP-PARAM PLAN-CONCAT CAP-CUR!  CAP-PEND-RESET ;

: CAP-GATHER ( -- )
   1 CAP-NEED  CAP-CUR@ CAP-PARAM PLAN-GATHER CAP-CUR!  CAP-PEND-RESET ;

\ ---- bridge the captured plan into the model-IR node table -----------------
: PLAN-REF ( tensor -- n )                      \ plan tensor handle -> MIR operand ref
   tensor>N {: h:n :}
   h CAP-IN-N @ < if h MIR-IN-REF else h CAP-IN-N @ - then ;

\ movement nodes materialize only on a materialize/gathered verdict; compute nodes
\ stay materialized (the conservative cad-1 default until the fusion planner lands).
: BRIDGE-MAT ( n n -- n ) {: op:n attr:n :}     \ op-kind attr -> materialization flag
   op OPR-CLASS CLASS-MOVEMENT = if
      attr MV-VD@ MV-VD-REPORTS? if 1 else 0 then
   else 1 then ;

: BRIDGE-NODE ( n -- ) {: j:n :}
   j PLAN-OP@ {: op:n :}
   op MIR-OP-BEGIN
   j PLAN-IN-COUNT@ 0 ?do  j i PLAN-IN@ PLAN-REF MIR-IN+  loop
   j PLAN-OUT@ {: y:tensor :}
   j PLAN-ATTR@ {: attr:n :}
   y TV-ROWS@ y TV-COLS@ y TV-DTYPE@ y TV-LAYOUT@  attr  op attr BRIDGE-MAT  MIR-OP+ drop ;

: BRIDGE-PLAN ( -- )  PLAN-N@ 0 ?do i BRIDGE-NODE loop ;

: CAP-END ( -- )
   CAP-PEND-CNT 0 > if E-CAD-REF throw then         \ a named ref left unconsumed by any op
   PLAN-N@ 0= if E-CAD-EMPTY throw then
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

\ one "[name:]RxC" spec: declare the input and (when named) bind its reference
: SIG-INPUT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SPEC-NAME  a u PARSE-SHAPE  CAP-INPUT-NAMED ;

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

\ movement token carrying colon params: RESHAPE:RxC | SLICE:R0..R1
: CAP-MOVE-PARAM ( n ptr u8 n -- ) {: op:n a:ptr u:n :}
   op OP-RESHAPE = if a u PARSE-SHAPE CAP-RESHAPE exit then
   op OP-SLICE   = if a u PARSE-RANGE CAP-SLICE   exit then
   E-CAD-SYNTAX throw ;                               \ others take no colon params

\ param-less movement token: TRANSPOSE | CONCAT | GATHER
: CAP-MOVE0 ( n -- ) {: op:n :}
   op OP-TRANSPOSE = if CAP-TRANSPOSE exit then
   op OP-CONCAT    = if CAP-CONCAT    exit then
   op OP-GATHER    = if CAP-GATHER    exit then
   E-CAD-SYNTAX throw ;                               \ reshape/slice require params

\ one body token: named-value reference, compute op, param-less movement, or "MOVE:params"
: CAP-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NT-FIND if  CAP-PEND-PUSH  exit  then  drop   \ known name -> pending operand ref
   a u $3A INDEX-OF {: ci:n :}
   ci 0< if
      a u OP-KIND {: op:n :}
      op OPR-CLASS CLASS-MOVEMENT = if op CAP-MOVE0 else op CAP-OP then
      exit
   then
   a ci OP-KIND  a ci 1+ +  u ci 1+ -  CAP-MOVE-PARAM ;

\ ">V NAME": bind NAME to the current value (read the name token from the body)
: PARSE-NAMED ( -- )
   parse-name dup 0= if 2drop E-CAD-SYNTAX throw then  NT-BIND-CUR ;

: PARSE-BODY ( -- )                             \ op / ">V NAME" / reference tokens up to ';'
   begin
      parse-name dup 0= if 2drop E-CAD-SYNTAX throw then
      2dup s" ;"  STR= if 2drop CAP-END exit then
      2dup s" >V" STR= if 2drop PARSE-NAMED else CAP-TOKEN then
   again ;

public

\ MODEL: NAME ( inputs -- outputs ) OP OP ... ;   (single line)
: MODEL: ( -- )
   CAP-BEGIN
   parse-name dup 0= if 2drop E-CAD-EMPTY throw then MIR-NAME!
   PARSE-SIG
   PARSE-BODY ;

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
: RB-REF-LAY ( n -- n ) {: r:n :}
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
   nd MIR-OP@ {: op:n :}
   op OP-RESHAPE   = if nd RB-RESHAPE   exit then
   op OP-TRANSPOSE = if nd RB-TRANSPOSE exit then
   op OP-SLICE     = if nd RB-SLICE     exit then
   op OP-CONCAT    = if nd RB-CONCAT    exit then
   op OP-GATHER    = if nd RB-GATHER    exit then
   E-CAD-BIND-SHAPE throw ;

\ movement dissolution verdict re-derived from the new extents (slice re-checks its
\ offset/col alignment; the rest are layout- or constant-determined).
: RB-VD-RESHAPE ( n -- n ) {: nd:n :}  nd 0 MIR-IN@ RB-REF-LAY MV-RESHAPE-VERDICT ;
: RB-VD-SLICE ( n n -- n ) {: nd:n cols:n :}
   nd 0 MIR-IN@ RB-REF-LAY  nd MIR-ATTR@ MV-PA@  cols  MV-SLICE-VERDICT ;
: RB-MOVE-VD ( n n -- n ) {: nd:n cols:n :}
   nd MIR-OP@ {: op:n :}
   op OP-RESHAPE   = if nd RB-VD-RESHAPE     exit then
   op OP-TRANSPOSE = if MV-TRANSPOSE-VERDICT exit then
   op OP-SLICE     = if nd cols RB-VD-SLICE  exit then
   op OP-CONCAT    = if MV-CONCAT-VERDICT    exit then
   op OP-GATHER    = if MV-GATHER-VERDICT    exit then
   E-CAD-BIND-SHAPE throw ;

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
   x RB-REF-ROWS w RB-REF-COLS  b RB-REF-ROWS b RB-REF-COLS  OP-BIAS  SHP-CHECK ;

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
   MODEL-NAME$ RPT-MODEL!
   MODEL-K MODEL-K RPT-OPS!
   MODEL-K RPT-REGIONS!
   MIR-MAT-COUNT RPT-MATERIALIZED! ;

: LOWER-KEYS ( report -- report )              \ shape/dtype/layout of the model output
   MIR-N@ 0= if exit then
   MIR-N@ 1- {: out:n :}
   out MIR-SHAPE-KEY  RPT-SHAPE!
   out MIR-DTYPE-KEY  RPT-DTYPE!
   out MIR-LAYOUT-KEY RPT-LAYOUT! ;

: LOWER-INTO ( report -- report )
   CAD-BASE
   LOWER-KEYS
   s" lowering: model-IR node table (cad-1)" RPT-WARN+ ;

\ FUSE plans regions (maki/fusion-plan.f) then estimates traffic (maki/traffic.f):
\ ops before (nodes) / after (regions), the typed split rows, the materialized count
\ from the updated IR flags, and estimated bytes before/after when the shapes bind.
: FUSE-INTO ( report -- report )
   FP-BUILD
   MODEL-K FP-REGION-COUNT RPT-OPS!
   FP-REGION-COUNT RPT-REGIONS!
   MIR-MAT-COUNT RPT-MATERIALIZED!
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
   node MOVE-WARN$ RPT-WARN+ ;

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
      i FP-RID@ r =  i MIR-OP@ OP-SOFTMAX-ROW =  and if unloop true exit then
   loop false ;

: REGION-FAM ( n -- n ) {: r:n :}               \ region -> schedule family id
   r FP-REGION-CLASSMIX  r REGION-HAS-SOFTMAX?  FAM-SELECT ;

\ max legal vector width for the region output's compiler-allocated (AL-16) write;
\ this is the elementwise default's "max legal vec" (else scalar for a strided write).
: REGION-MAXVEC ( n -- n ) {: rep:n :}
   rep MIR-LAY@ LAY-ROW <> if 1 exit then
   AL-16  rep MIR-DT@ DT-SIZE  rep MIR-COLS@  MP-W ;

: TILE-CANDS+ ( report n -- report ) {: fam:n :}   \ emit every candidate row of a family
   fam FAM-SPACE 0 ?do  fam i CAND$ RPT-CAND+  loop ;

\ replay lookup is the cad-5 store seam: a miss means the shape class is unmeasured.
: TILE-REPLAY-NOTE ( report n -- report ) {: r:n :}
   r SK-KEY$ SK-GET nip if exit then
   s" schedule: unmeasured shape class -> using defaults" RPT-WARN+ ;

: TILE-INTO ( report -- report )
   FP-BUILD
   0 REGION-FAM {: fam:n :}
   0 SK-REGION-REP {: rep:n :}
   fam TILE-CANDS+
   fam  rep MIR-COLS@  rep REGION-MAXVEC  FAM-DEFAULT  RPT-SELECT!
   0 SK-KEY$ RPT-CACHE!
   0 TILE-REPLAY-NOTE
   s" schedule: defaults (unmeasured shape class - cad-6 tunes)" RPT-WARN+
   s" schedule: family from region 0 only (v1 limitation)" RPT-WARN+ ;

: TUNE-INTO ( report -- report )
   TILE-INTO
   s" tune: measurement needs device (cad-6)" RPT-WARN+ ;

: CERTIFY-INTO ( report -- report )            \ static, no GPU: model-level legality
   s" " V-PASS G-CERTIFY RPT-GATE!
   s" certify: model-level legality only; kernel legality in cad-5" RPT-WARN+ ;

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
   s" no-device" V-NOTRUN G-PROFILE RPT-GATE! ;

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
   LOWER-GOLDEN-REASON$ v G-GOLDEN RPT-GATE!
   s" golden: device model golden (cross-region vs host, composed licensed-precision tolerance)" RPT-WARN+ ;
: GOLDEN-GATE-INTO ( report -- report )
   GA-EXISTS? if GOLDEN-INTO exit then             \ external artifact wins (GOLDEN-INTO selects it)
   CUDA:OPEN? if
      FP-BUILD                                     \ the device legs read the region plan
      MDL-CUBINS-READY? if MDL-LOWERABLE? if GOLDEN-GATE-DEVICE exit then then
   then
   GOLDEN-INTO ;                                   \ host self-consistency (device flag cleared there)

\ full conservative report over every phase (PROMOTE / OPTIMIZE / EXPLAIN)
: FULL-REPORT ( -- report )
   RPT-NEW LOWER-INTO FUSE-INTO MEMORY-INTO TILE-INTO
   CERTIFY-INTO GOLDEN-GATE-INTO GRADCHECK-INTO PROFILE-INTO ;

\ ---- promotion gate (CAD 7c gate-set alignment) ----------------------------
: GATE-PASS? ( report n -- report bool )
   over swap RPT-GATE-TAG@ V-PASS = ;
: GATE-NOT-FAIL? ( report n -- report bool )   \ pass or not-run, but not a real fail
   over swap RPT-GATE-TAG@ V-FAIL <> ;
: GATE-RECORDED? ( report n -- report bool )   \ a recorded verdict (any legal tag)
   over swap RPT-GATE-TAG@ dup 0 >= swap V-N < and ;

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
   MODEL-NAME$ RPT-CACHE! ;

: PROMOTE-REPORT ( report -- report )
   PROMOTE-OK? 0= if E-CAD-GATE throw then
   CACHE-KEY-INTO ;

\ On a passing PROMOTE, write the artifact record to the CAD store (maki/store.f):
\ an evidence row (the four gate verdicts) and a schedules row (region-0 selection),
\ both keyed by the section 7.4 key of region 0 (the same key TILE/OPTIMIZE cache). A
\ refused promote throws in PROMOTE-REPORT before this runs, so no partial rows land.
: PROMOTE-EVIDENCE ( report -- report )
   dup G-CERTIFY   RPT-GATE-TAG@ {: c:n :}
   dup G-GOLDEN    RPT-GATE-TAG@ {: g:n :}
   dup G-GRADCHECK RPT-GATE-TAG@ {: gc:n :}
   dup G-PROFILE   RPT-GATE-TAG@ {: p:n :}
   dup RPT-SELECT@ {: sel:n :}
   0 SK-KEY$ c g gc p GOLDEN-DEV? GOLDEN-PREC@ EVID-PUT-G  \ golden=device-<v>:<prec> when the device leg ran
   0 SK-KEY$ sel SCHED-PUT ;

: OPTIMIZE-PROMOTE ( report -- report )        \ record the decision, never throw
   PROMOTE-OK? if
      CACHE-KEY-INTO  s" promote: gates pass; artifact cached" RPT-WARN+
   else
      s" promote: refused; certify/golden/gradcheck gate not satisfied" RPT-WARN+
   then ;

public

\ ---- inspection commands (each returns a structured cad-0a report) ----------
: LOWER ( -- report )      RPT-NEW LOWER-INTO ;
: FUSE ( -- report )       RPT-NEW LOWER-INTO FUSE-INTO ;
: MEMORY ( -- report )     RPT-NEW LOWER-INTO MEMORY-INTO ;
: TILE ( -- report )       RPT-NEW LOWER-INTO TILE-INTO ;
: CERTIFY ( -- report )    RPT-NEW LOWER-INTO CERTIFY-INTO ;
: GOLDEN ( -- report )     RPT-NEW LOWER-INTO GOLDEN-GATE-INTO ;
: GRADCHECK ( -- report )  RPT-NEW LOWER-INTO GRADCHECK-INTO ;
: PROFILE ( -- report )    RPT-NEW LOWER-INTO PROFILE-INTO ;
: TUNE ( -- report )       RPT-NEW LOWER-INTO TUNE-INTO ;

\ PROMOTE refuses (named throw) unless every gate passes; on success caches the key
\ and writes the artifact + evidence rows to the CAD store.
: PROMOTE ( -- report )  FULL-REPORT PROMOTE-REPORT PROMOTE-EVIDENCE ;

\ OPTIMIZE composes lower -> fuse -> memory -> tile -> gates -> promote decision.
: OPTIMIZE ( -- report )  FULL-REPORT OPTIMIZE-PROMOTE ;

\ EXPLAIN emits repair-packet-discipline failure lines for every non-pass gate.
: EXPLAIN ( -- ptr u8 n )  FULL-REPORT RPT-RENDER-PACKETS ;

\ CAD-SHOW renders a report's machine view to stdout (interactive convenience).
: CAD-SHOW ( report -- )  RPT-RENDER type cr ;

end-package
