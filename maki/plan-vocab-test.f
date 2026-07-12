\ maki/plan-vocab-test.f - CAD-PLAN section 3 over the NAMED planning vocabulary (maki/plan-vocab.f).
\
\ maki/plan-compose-test.f proved the section-3 form using the PARAMETRIC appenders (op-kind
\ threaded as an OP-* argument). This file proves the SAME thing over the named vocabulary: a
\ model block opens `package PLAN` and its bare LINEAR / GELU / RESIDUAL-ADD read as section-3
\ source, and the checker certifies the WHOLE composition (arity + tensor discipline) at author
\ time. The skip and fan-out DAG forms of plan-compose-test are migrated onto the named words.
\
\ Two proofs, both at LOAD time:
\   1. Positive: PVT-SKIP / PVT-BRANCH are top-level ": ... ;" definitions - bin/hb checks their
\      ( tensor ... -- tensor ) effects on load, and the drivers run them to assert the captured
\      plan (op sequence, re-rooted skip operand, fan-out, shape flow) matches plan-compose-test.
\   2. Negative: EVAL:CHECK-PASSES? (the same checker, driven over a source string) proves a malformed
\      composition - wrong arity, a non-tensor operand, a leftover value, a movement op missing its
\      scalar params - is REJECTED before any planning. Run inside `package PLAN` so the candidate's
\      bare vocabulary words resolve; the positive controls double as the scope guard.
\
\ NOT the MODEL:-driven capture: having MODEL: (an executing word) open this package and compile its
\ body as a checked ": ... ;" needs checker reentrancy (dot habu-checker-reentrancy-certify-86771a6f).
\ This file locks the hand-authored half that IS expressible today. maki -> habu only.

require lib/test.f
require maki/eval.f          \ EVAL:CHECK-PASSES?: drive the checker over a candidate string
require maki/plan-vocab.f
require maki/tensor-value.f

\ ---- model blocks authored as ordinary checked words over the NAMED vocabulary ----
\ Bare LINEAR / GELU / RESIDUAL-ADD / RMSNORM / ADD resolve to the PLAN package words; the
\ block reads as section-3 source and the checker certifies its arity before any planning runs.
package PLAN
public

\ PVT-SKIP: linear -> gelu -> linear -> residual(+x) -> rmsnorm. The residual re-roots its PARAM
\ onto the ORIGINAL input x (a true skip); x fans out to the first linear (node 0) and the residual
\ (node 3). Same DAG as plan-compose-test's PCT-SKIP, now over the named vocabulary.
: PVT-SKIP ( tensor tensor tensor tensor tensor -- tensor ) {: x:tensor w1:tensor b1:tensor w2:tensor b2:tensor :}
   x w1 b1 LINEAR                       \ n0: linear(x, w1, b1)
   GELU                                 \ n1: gelu(n0)
   w2 b2 LINEAR                         \ n2: linear(n1, w2, b2)
   x RESIDUAL-ADD                       \ n3: residual(n2, x)   <- x re-rooted as the param
   RMSNORM ;                            \ n4: rmsnorm(n3)

\ PVT-BRANCH: two branches from x join in an add - a DAG. GELU's data operand is x (re-root), the
\ linear also reads x (fan-out), and the add joins both branch outputs.
: PVT-BRANCH ( tensor tensor tensor -- tensor ) {: x:tensor w:tensor b:tensor :}
   x GELU {: g:tensor :}                \ n0: gelu(x)          <- re-root: data = x
   x w b LINEAR {: h:tensor :}          \ n1: linear(x, w, b)  <- x fans out
   h g ADD ;                            \ n2: add(n1, n0)      <- branches join

;package

\ ---- descriptor seeding + plan-store probes + scenario drivers -------------------
\ MAKI scope: the plan store, descriptor constructor and op-kind constants are MAKI words; the
\ drivers call the PLAN blocks as PLAN:PVT-SKIP / PLAN:PVT-BRANCH.
package MAKI

: PVT-DESC ( n n -- tensor ) {: rows:n cols:n :}   \ f32 row-major planning descriptor
   rows cols SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC ;
: PVT-IN ( n n -- tensor ) {: node:n k:n :}  node k TENSOR:PLAN-IN@ ;   \ k-th input handle
: PVT-OUT ( n -- tensor ) {: node:n :}  node TENSOR:PLAN-OUT@ ;         \ output handle

\ PVT-SKIP: a checker-verified 5-node plan with the residual re-rooted onto x (skip) and x
\ fanning out to node 0 and node 3.
: PVT-RUN-SKIP ( -- )
   TENSOR:TV-RESET TENSOR:PLAN-RESET
   4 8 PVT-DESC {: x:tensor :}          \ handle 0
   8 16 PVT-DESC {: w1:tensor :}        \ handle 1
   1 16 PVT-DESC {: b1:tensor :}        \ handle 2
   16 8 PVT-DESC {: w2:tensor :}        \ handle 3
   1 8 PVT-DESC {: b2:tensor :}         \ handle 4
   x w1 b1 w2 b2 PLAN:PVT-SKIP {: y:tensor :}
   TENSOR:PLAN-N@ 5 T=                         \ five IR nodes captured
   0 TENSOR:PLAN-OP@ OPKIND>N OP-LINEAR       T=
   1 TENSOR:PLAN-OP@ OPKIND>N OP-GELU         T=
   2 TENSOR:PLAN-OP@ OPKIND>N OP-LINEAR       T=
   3 TENSOR:PLAN-OP@ OPKIND>N OP-RESIDUAL-ADD T=
   4 TENSOR:PLAN-OP@ OPKIND>N OP-RMSNORM      T=
   3 TENSOR:PLAN-IN-COUNT@ 2 T=
   0 0 PVT-IN x TENSOR:TV-EQUAL? TTRUE             \ node0.in0 = x
   3 1 PVT-IN x TENSOR:TV-EQUAL? TTRUE             \ node3.in1 = x   (the skip)
   3 0 PVT-IN 2 PVT-OUT TENSOR:TV-EQUAL? TTRUE            \ node3.in0 = node2 output (data = running value)
   y TENSOR:TV-ROWS@ ROWS-RAW 4 T=  y TENSOR:TV-COLS@ COLS-RAW 8 T=     \ shape flows through the whole composition
   4 TENSOR:PLAN-OUT@ TENSOR:TV-ROWS@ ROWS-RAW 4 T=  4 TENSOR:PLAN-OUT@ TENSOR:TV-COLS@ COLS-RAW 8 T= ;

\ PVT-BRANCH: a DAG (re-root + fan-out + join).
: PVT-RUN-BRANCH ( -- )
   TENSOR:TV-RESET TENSOR:PLAN-RESET
   4 8 PVT-DESC {: x:tensor :}          \ handle 0
   8 8 PVT-DESC {: w:tensor :}          \ handle 1
   1 8 PVT-DESC {: b:tensor :}          \ handle 2
   x w b PLAN:PVT-BRANCH {: y:tensor :}
   TENSOR:PLAN-N@ 3 T=
   0 TENSOR:PLAN-OP@ OPKIND>N OP-GELU   T=
   1 TENSOR:PLAN-OP@ OPKIND>N OP-LINEAR T=
   2 TENSOR:PLAN-OP@ OPKIND>N OP-ADD    T=
   0 0 PVT-IN x TENSOR:TV-EQUAL? TTRUE             \ node0.in0 = x   (gelu re-rooted onto x)
   1 0 PVT-IN x TENSOR:TV-EQUAL? TTRUE             \ node1.in0 = x   (x fanned out again)
   2 0 PVT-IN 1 PVT-OUT TENSOR:TV-EQUAL? TTRUE            \ node2.in0 = linear output
   2 1 PVT-IN 0 PVT-OUT TENSOR:TV-EQUAL? TTRUE            \ node2.in1 = gelu output
   2 TENSOR:PLAN-IN-COUNT@ 2 T=
   y TENSOR:TV-ROWS@ ROWS-RAW 4 T=  y TENSOR:TV-COLS@ COLS-RAW 8 T= ;

;package

T-RESET

\ ---- proof 1: the named-vocabulary compositions capture the right plan --------
package MAKI
PVT-RUN-SKIP
PVT-RUN-BRANCH
;package

\ ---- proof 2: the checker REJECTS malformed compositions at load (EVAL:CHECK-PASSES?) --
\ Inside package PLAN so the candidate's bare vocabulary words resolve. Positive controls
\ certify (and prove the words resolve); every malformed form is rejected before planning.
package PLAN
\ positive controls: well-formed compositions certify (arity + tensor discipline hold)
s" PVOK-LIN ( tensor tensor tensor -- tensor ) LINEAR GELU"                                   EVAL:CHECK-PASSES? TTRUE
s" PVOK-SKIP ( tensor tensor tensor tensor tensor -- tensor ) {: x w1 b1 w2 b2 :} x w1 b1 LINEAR GELU w2 b2 LINEAR x RESIDUAL-ADD RMSNORM"  EVAL:CHECK-PASSES? TTRUE
s" PVOK-RESHAPE ( tensor CAD-KIND:rows CAD-KIND:cols -- tensor ) RESHAPE"                     EVAL:CHECK-PASSES? TTRUE
\ a raw-n reshape target is a checker reject (Model-CAD V2 R3 nominal extents)
s" PVBAD-RESHAPE-N ( tensor n n -- tensor ) RESHAPE"                                          EVAL:CHECK-PASSES? TFALSE
\ negatives: arity underflow (binary / ternary / movement ops missing operands)
s" PVBAD-ADD ( tensor -- tensor ) ADD"                                                        EVAL:CHECK-PASSES? TFALSE
s" PVBAD-LINEAR ( tensor tensor -- tensor ) LINEAR"                                           EVAL:CHECK-PASSES? TFALSE
s" PVBAD-RESHAPE ( tensor -- tensor ) RESHAPE"                                                EVAL:CHECK-PASSES? TFALSE
\ negative: type mismatch (a non-tensor value fed to a tensor-typed op)
s" PVBAD-TYPE ( n -- tensor ) GELU"                                                           EVAL:CHECK-PASSES? TFALSE
\ negative: leftover value (two inputs, one unary op) - result arity != declared output
s" PVBAD-LEFT ( tensor tensor -- tensor ) GELU"                                               EVAL:CHECK-PASSES? TFALSE
;package

T-REPORT
