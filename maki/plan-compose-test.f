\ maki/plan-compose-test.f - CAD-PLAN section 3, achievable half: a model block is an
\ ORDINARY checked word over tensor descriptors, and the checker verifies the WHOLE
\ composition (stack arity + tensor discipline) at author time.
\
\ The blocks below are top-level ": ... ;" definitions checked by bin/hb on load: their
\ ( tensor ... -- tensor ) signatures are typed stack effects, so a wrong-arity or
\ non-tensor composition is a CHECKER diagnostic before any planning runs (proven by
\ the negative reproducers recorded on dot habu-compiler-pkg-scoped-1a4d29bd). This is
\ the concrete embodiment of section 3's "the checker checks model words as ordinary
\ words over descriptor types" and "a model block is a word; blocks compose by
\ concatenation" - realised for HAND-AUTHORED top-level blocks.
\
\ It is NOT the MODEL:-driven capture: having MODEL: (an executing word) compile its
\ body as a ": ... ;" and have the check hook certify it CRASHES the native engine
\ (checker non-reentrancy across a word-execution boundary). That gap, and the
\ shape/kind-opacity of the single `tensor` type, are recorded as compiler-capability
\ requirements on the dot; this file locks the part that IS expressible today.
\
\ The stack-composition form ALSO lifts the v1 capture data-operand limitation ("a named
\ ref supplies PARAMETER operands only; the data operand is always the running value").
\ On the stack ANY value can be an op's DATA operand, so a block freely RE-ROOTS the
\ chain (PCT-BRANCH applies GELU to the ORIGINAL input, not the running value - the exact
\ "H1 GELU" the linear-consumption capture rejects with E-CAD-REF) and FANS a value out
\ to several consumers (a true DAG). maki -> habu only.

require lib/test.f
require maki/plan-ops.f
require maki/tensor-value.f

package MAKI

\ ---- model blocks authored as ordinary checked words over descriptors ----------
\ PCT-SKIP: linear -> gelu -> linear -> residual(+x) -> rmsnorm, with the residual
\ re-rooting its PARAM onto the ORIGINAL input x (a true skip) and x fanning out to the
\ first linear (node 0) and the residual (node 3). The checker certifies the arity of
\ the whole block before any planning; each op returns a descriptor so the words compose.
: PCT-SKIP ( tensor tensor tensor tensor tensor -- tensor ) {: x:tensor w1:tensor b1:tensor w2:tensor b2:tensor :}
   x w1 b1 OP-LINEAR PLAN-LINEAR        \ n0: linear(x, w1, b1)
   OP-GELU PLAN-UNARY                   \ n1: gelu(n0)
   w2 b2 OP-LINEAR PLAN-LINEAR          \ n2: linear(n1, w2, b2)
   x OP-RESIDUAL-ADD PLAN-BIN-EW        \ n3: residual(n2, x)   <- x re-rooted as param
   OP-RMSNORM PLAN-UNARY ;              \ n4: rmsnorm(n3)

\ PCT-BRANCH: two branches from x join in an add - a DAG the linear-consumption capture
\ cannot express. GELU's DATA operand is x itself (re-root, not the running value), the
\ linear also reads x (fan-out), and the add joins both branch outputs.
: PCT-BRANCH ( tensor tensor tensor -- tensor ) {: x:tensor w:tensor b:tensor :}
   x OP-GELU PLAN-UNARY {: g:tensor :}          \ n0: gelu(x)          <- re-root: data = x
   x w b OP-LINEAR PLAN-LINEAR {: h:tensor :}   \ n1: linear(x, w, b)  <- x fans out
   h g OP-ADD PLAN-BIN-EW ;             \ n2: add(n1, n0)      <- branches join

\ ---- descriptor seeding + plan-store probes (read the captured IR node facts) ----
: PCT-DESC ( n n -- tensor ) {: rows:n cols:n :}   \ f32 row-major planning descriptor
   rows cols MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW TENSOR:TV-DESC ;
: PCT-IN ( n n -- n ) {: node:n k:n :}  node k TENSOR:PLAN-IN@ TENSOR:tensor>N ;   \ k-th input handle
: PCT-OUT ( n -- n ) {: node:n :}  node TENSOR:PLAN-OUT@ TENSOR:tensor>N ;         \ output handle

\ ---- scenario drivers (seed descriptors, run the block, assert the captured plan) ----
\ PCT-SKIP: a checker-verified 5-node plan with the residual re-rooted onto x (skip) and
\ x fanning out to node 0 and node 3.
: PCT-RUN-SKIP ( -- )
   TENSOR:TV-RESET TENSOR:PLAN-RESET
   4 8 PCT-DESC {: x:tensor :}          \ handle 0
   8 16 PCT-DESC {: w1:tensor :}        \ handle 1
   1 16 PCT-DESC {: b1:tensor :}        \ handle 2
   16 8 PCT-DESC {: w2:tensor :}        \ handle 3
   1 8 PCT-DESC {: b2:tensor :}         \ handle 4
   x w1 b1 w2 b2 PCT-SKIP {: y:tensor :}
   TENSOR:PLAN-N@ 5 T=                         \ five IR nodes captured
   0 TENSOR:PLAN-OP@ OP-LINEAR       T=
   1 TENSOR:PLAN-OP@ OP-GELU         T=
   2 TENSOR:PLAN-OP@ OP-LINEAR       T=
   3 TENSOR:PLAN-OP@ OP-RESIDUAL-ADD T=
   4 TENSOR:PLAN-OP@ OP-RMSNORM      T=
   3 TENSOR:PLAN-IN-COUNT@ 2 T=
   0 0 PCT-IN x TENSOR:tensor>N T=             \ node0.in0 = x
   3 1 PCT-IN x TENSOR:tensor>N T=             \ node3.in1 = x   (the skip)
   3 0 PCT-IN 2 PCT-OUT   T=            \ node3.in0 = node2 output (data = running value)
   y TENSOR:TV-ROWS@ 4 T=  y TENSOR:TV-COLS@ 8 T=     \ shape flows through the whole composition
   4 TENSOR:PLAN-OUT@ TENSOR:TV-ROWS@ 4 T=  4 TENSOR:PLAN-OUT@ TENSOR:TV-COLS@ 8 T= ;

\ PCT-BRANCH: a DAG (re-root + fan-out + join) the v1 capture cannot express.
: PCT-RUN-BRANCH ( -- )
   TENSOR:TV-RESET TENSOR:PLAN-RESET
   4 8 PCT-DESC {: x:tensor :}          \ handle 0
   8 8 PCT-DESC {: w:tensor :}          \ handle 1
   1 8 PCT-DESC {: b:tensor :}          \ handle 2
   x w b PCT-BRANCH {: y:tensor :}
   TENSOR:PLAN-N@ 3 T=
   0 TENSOR:PLAN-OP@ OP-GELU   T=
   1 TENSOR:PLAN-OP@ OP-LINEAR T=
   2 TENSOR:PLAN-OP@ OP-ADD    T=
   0 0 PCT-IN x TENSOR:tensor>N T=             \ node0.in0 = x   (gelu re-rooted onto x)
   1 0 PCT-IN x TENSOR:tensor>N T=             \ node1.in0 = x   (x fanned out again)
   2 0 PCT-IN 1 PCT-OUT   T=            \ node2.in0 = linear output
   2 1 PCT-IN 0 PCT-OUT   T=            \ node2.in1 = gelu output
   2 TENSOR:PLAN-IN-COUNT@ 2 T=
   y TENSOR:TV-ROWS@ 4 T=  y TENSOR:TV-COLS@ 8 T= ;

T-RESET
PCT-RUN-SKIP
PCT-RUN-BRANCH
T-REPORT

end-package
