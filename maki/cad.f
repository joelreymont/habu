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
\ Fail closed: unknown op token -> E-CAD-OP; empty body -> E-CAD-EMPTY; malformed
\ signature/shape -> E-CAD-SYNTAX; an op with no data or too few declared inputs ->
\ E-CAD-ARITY; too many inputs -> E-CAD-INPUTS. LOWER reports REAL node facts (op
\ count + shape/dtype/layout keys from the IR); FUSE plans real regions + traffic
\ (maki/fusion-plan.f, maki/traffic.f); MEMORY plans per-hot coalescing status +
\ vector-width/tail facts (maki/mem-plan.f, cad-3); TILE stays conservative (cad-4).
\ GOLDEN/GRADCHECK/PROFILE stay honest
\ not-run on a host without a GPU. PROMOTE refuses (E-CAD-GATE) unless all gates
\ pass. maki -> habu only; cad owns -5020..-5029.

require lib/string.f
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

-5020 constant E-CAD-NOMODEL   \ command issued with no model defined
-5021 constant E-CAD-OP        \ unknown op token in a MODEL: body
-5022 constant E-CAD-EMPTY     \ MODEL: with no name or no ops
-5023 constant E-CAD-GATE      \ PROMOTE refused: required gates did not pass
-5024 constant E-CAD-SYNTAX    \ malformed MODEL: (bad signature / shape / terminator)
-5025 constant E-CAD-ARITY     \ op missing its data input or declared parameters
-5026 constant E-CAD-INPUTS    \ more model inputs than the capture pool holds

package MAKI
private

64 constant MODEL-NAME-CAP
create MODEL-NAME MODEL-NAME-CAP allot   variable MODEL-NAME-U
variable MODEL-SET?                        \ 0 until a MODEL: succeeds

64 constant CAP-CAP                        \ max model inputs (matches model-ir slots)
create CAP-INS CAP-CAP cells allot         \ declared input tensor handles (as n)
variable CAP-IN-N
variable CAP-CUR                           \ running "current" tensor handle (n; -1 = none)
variable CAP-IP                            \ next unconsumed parameter-input index

public

\ ---- op-name -> op-kind (fail closed: unknown op is rejected, never guessed) -
: OP-KIND ( ptr u8 n -- n )
   2dup s" ADD"          STR= if 2drop OP-ADD          exit then
   2dup s" MUL"          STR= if 2drop OP-MUL          exit then
   2dup s" SCALE"        STR= if 2drop OP-SCALE        exit then
   2dup s" BIAS"         STR= if 2drop OP-BIAS         exit then
   2dup s" RELU"         STR= if 2drop OP-RELU         exit then
   2dup s" GELU"         STR= if 2drop OP-GELU         exit then
   2dup s" SILU"         STR= if 2drop OP-SILU         exit then
   2dup s" LAYERNORM"    STR= if 2drop OP-LAYERNORM    exit then
   2dup s" RMSNORM"      STR= if 2drop OP-RMSNORM      exit then
   2dup s" SOFTMAX-ROW"  STR= if 2drop OP-SOFTMAX-ROW  exit then
   2dup s" MATMUL"       STR= if 2drop OP-MATMUL       exit then
   2dup s" LINEAR"       STR= if 2drop OP-LINEAR       exit then
   2dup s" RESIDUAL-ADD" STR= if 2drop OP-RESIDUAL-ADD exit then
   2dup s" CAST"         STR= if 2drop OP-CAST         exit then
   2dup s" ROPE"         STR= if 2drop OP-ROPE         exit then
   2dup s" RESHAPE"      STR= if 2drop OP-RESHAPE      exit then
   2dup s" TRANSPOSE"    STR= if 2drop OP-TRANSPOSE    exit then
   2dup s" SLICE"        STR= if 2drop OP-SLICE        exit then
   2dup s" CONCAT"       STR= if 2drop OP-CONCAT       exit then
   2dup s" GATHER"       STR= if 2drop OP-GATHER       exit then
   2drop E-CAD-OP throw ;

private

: MODEL-NAME! ( ptr u8 n -- )                  \ copy the transient parse-name token
   {: a:ptr u:n :}
   u MODEL-NAME-CAP > if E-CAD-SYNTAX throw then
   0 begin dup u < while  dup a + c@  over MODEL-NAME + c!  1+  repeat drop
   u MODEL-NAME-U ! ;

\ ---- capture engine --------------------------------------------------------
: CAP-IN@   ( n -- tensor )  cells CAP-INS + @ >tensor ;
: CAP-CUR@  ( -- tensor )    CAP-CUR @ >tensor ;
: CAP-CUR!  ( tensor -- )    tensor>N CAP-CUR ! ;

: CAP-BEGIN ( -- )
   TV-RESET  PLAN-RESET  MIR-RESET
   0 CAP-IN-N !  1 CAP-IP !  -1 CAP-CUR !
   0 MODEL-NAME-U !  0 MODEL-SET? ! ;

: CAP-INPUT ( n n -- ) {: rows:n cols:n :}      \ declare one model input (f32/row)
   CAP-IN-N @ CAP-CAP >= if E-CAD-INPUTS throw then
   rows cols DT-F32 LAY-ROW TV-DESC {: t:tensor :}
   rows cols DT-F32 LAY-ROW MIR-INPUT+ drop
   t tensor>N  CAP-INS CAP-IN-N @ cells + !
   CAP-IN-N @ 0= if t CAP-CUR! then               \ first input is the running value
   CAP-IN-N @ 1+ CAP-IN-N ! ;

: CAP-NEED ( n -- ) {: params:n :}              \ data + params must be available
   CAP-CUR @ 0< if E-CAD-ARITY throw then
   CAP-IP @ params + CAP-IN-N @ > if E-CAD-ARITY throw then ;

: CAP-P1 ( -- tensor )  CAP-IP @    CAP-IN@ ;
: CAP-P2 ( -- tensor )  CAP-IP @ 1+ CAP-IN@ ;

: CAP-OP ( n -- ) {: op:n :}                    \ apply one op-kind to the current value
   op OPR-ARITY {: ar:n :}
   ar 1- CAP-NEED
   op OPR-CLASS CLASS-MATMUL = {: mm:bool :}
   ar 1 = if
      CAP-CUR@ op PLAN-UNARY CAP-CUR!
   else ar 2 = if
      mm if CAP-CUR@ CAP-P1 op PLAN-MATMUL else CAP-CUR@ CAP-P1 op PLAN-BIN-EW then CAP-CUR!
   else ar 3 = if
      mm if CAP-CUR@ CAP-P1 CAP-P2 op PLAN-LINEAR else CAP-CUR@ CAP-P1 CAP-P2 op PLAN-TERN-EW then CAP-CUR!
   else
      E-CAD-ARITY throw
   then then then
   ar 1- CAP-IP @ + CAP-IP ! ;

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
   1 CAP-NEED  CAP-CUR@ CAP-P1 PLAN-CONCAT CAP-CUR!  CAP-IP @ 1+ CAP-IP ! ;

: CAP-GATHER ( -- )
   1 CAP-NEED  CAP-CUR@ CAP-P1 PLAN-GATHER CAP-CUR!  CAP-IP @ 1+ CAP-IP ! ;

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

: PARSE-SIG ( -- )                              \ '(' input-specs [ -- names ] ')'
   parse-name dup 0= if 2drop E-CAD-SYNTAX throw then
   s" (" STR= 0= if E-CAD-SYNTAX throw then
   begin
      parse-name dup 0= if 2drop E-CAD-SYNTAX throw then
      2dup s" --" STR= if 2drop SKIP-TO-RPAREN exit then
      2dup s" )"  STR= if 2drop exit then
      PARSE-SHAPE CAP-INPUT
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

\ one body token: compute op, param-less movement, or "MOVE:params"
: CAP-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   a u $3A INDEX-OF {: ci:n :}
   ci 0< if
      a u OP-KIND {: op:n :}
      op OPR-CLASS CLASS-MOVEMENT = if op CAP-MOVE0 else op CAP-OP then
      exit
   then
   a ci OP-KIND  a ci 1+ +  u ci 1+ -  CAP-MOVE-PARAM ;

: PARSE-BODY ( -- )                             \ op tokens up to ';'
   begin
      parse-name dup 0= if 2drop E-CAD-SYNTAX throw then
      2dup s" ;" STR= if 2drop CAP-END exit then
      CAP-TOKEN
   again ;

public

\ MODEL: NAME ( inputs -- outputs ) OP OP ... ;   (single line)
: MODEL: ( -- )
   CAP-BEGIN
   parse-name dup 0= if 2drop E-CAD-EMPTY throw then MODEL-NAME!
   PARSE-SIG
   PARSE-BODY ;

: MODEL-CLEAR ( -- )  CAP-BEGIN ;
: MODEL-DEFINED? ( -- bool )  MODEL-SET? @ 0= 0= ;
: MODEL-NAME$ ( -- ptr u8 n )  MODEL-NAME MODEL-NAME-U @ ;
: MODEL-K ( -- n )  MIR-N@ ;

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

: TILE-INTO ( report -- report )
   s" host-reference-v0" RPT-CAND+  0 RPT-SELECT!
   s" schedule: single host-reference candidate; autotuner in cad-4" RPT-WARN+ ;

: TUNE-INTO ( report -- report )
   TILE-INTO
   s" tune: no measurement history yet (cad-4)" RPT-WARN+ ;

: CERTIFY-INTO ( report -- report )            \ static, no GPU: model-level legality
   s" " V-PASS G-CERTIFY RPT-GATE!
   s" certify: model-level legality only; kernel legality in cad-5" RPT-WARN+ ;

: GOLDEN-INTO ( report -- report )
   s" no-device" V-NOTRUN G-GOLDEN RPT-GATE! ;

: GRADCHECK-INTO ( report -- report )
   s" no-device" V-NOTRUN G-GRADCHECK RPT-GATE! ;

: PROFILE-INTO ( report -- report )
   s" no-device" V-NOTRUN G-PROFILE RPT-GATE! ;

\ full conservative report over every phase (PROMOTE / OPTIMIZE / EXPLAIN)
: FULL-REPORT ( -- report )
   RPT-NEW LOWER-INTO FUSE-INTO MEMORY-INTO TILE-INTO
   CERTIFY-INTO GOLDEN-INTO GRADCHECK-INTO PROFILE-INTO ;

\ ---- promotion gate --------------------------------------------------------
: GATE-PASS? ( report n -- report bool )
   over swap RPT-GATE-TAG@ V-PASS = ;

: PROMOTE-OK? ( report -- report bool )        \ all four gates pass
   G-CERTIFY   GATE-PASS? >r
   G-GOLDEN    GATE-PASS? r> and >r
   G-GRADCHECK GATE-PASS? r> and >r
   G-PROFILE   GATE-PASS? r> and ;

: CACHE-KEY-INTO ( report -- report )          \ artifact key (model-scoped in phase 1)
   MODEL-NAME$ RPT-CACHE! ;

: PROMOTE-REPORT ( report -- report )
   PROMOTE-OK? 0= if E-CAD-GATE throw then
   CACHE-KEY-INTO ;

: OPTIMIZE-PROMOTE ( report -- report )        \ record the decision, never throw
   PROMOTE-OK? if
      CACHE-KEY-INTO  s" promote: gates pass; artifact cached" RPT-WARN+
   else
      s" promote: refused; required device gates not run on host" RPT-WARN+
   then ;

public

\ ---- inspection commands (each returns a structured cad-0a report) ----------
: LOWER ( -- report )      RPT-NEW LOWER-INTO ;
: FUSE ( -- report )       RPT-NEW LOWER-INTO FUSE-INTO ;
: MEMORY ( -- report )     RPT-NEW LOWER-INTO MEMORY-INTO ;
: TILE ( -- report )       RPT-NEW LOWER-INTO TILE-INTO ;
: CERTIFY ( -- report )    RPT-NEW LOWER-INTO CERTIFY-INTO ;
: GOLDEN ( -- report )     RPT-NEW LOWER-INTO GOLDEN-INTO ;
: GRADCHECK ( -- report )  RPT-NEW LOWER-INTO GRADCHECK-INTO ;
: PROFILE ( -- report )    RPT-NEW LOWER-INTO PROFILE-INTO ;
: TUNE ( -- report )       RPT-NEW LOWER-INTO TUNE-INTO ;

\ PROMOTE refuses (named throw) unless every gate passes; on success caches.
: PROMOTE ( -- report )  FULL-REPORT PROMOTE-REPORT ;

\ OPTIMIZE composes lower -> fuse -> memory -> tile -> gates -> promote decision.
: OPTIMIZE ( -- report )  FULL-REPORT OPTIMIZE-PROMOTE ;

\ EXPLAIN emits repair-packet-discipline failure lines for every non-pass gate.
: EXPLAIN ( -- ptr u8 n )  FULL-REPORT RPT-RENDER-PACKETS ;

\ CAD-SHOW renders a report's machine view to stdout (interactive convenience).
: CAD-SHOW ( report -- )  RPT-RENDER type cr ;

end-package
