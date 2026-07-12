\ maki/mlp-bwd-test.f - the cad-9e MLP backward end-to-end proof.
\
\ The flagship LINEAR GELU LINEAR model builds a full backward IR (matmul adjoints +
\ gelu-bwd + the two bias row-reduces), with every parameter-gradient SHAPE asserted -
\ this is the capability habu-maki-from-scratch depends on (an MLP needs the linear
\ adjoint). Gradcheck now RUNS on host (cad-7a full-tensor executor): the MLP and the
\ bias/scale/gather models all gradcheck V-PASS (analytic backward == finite diff),
\ NEVER a false pass. Only a non-differentiable op (cast) stays honest not-run.

require lib/test.f
require lib/string.f
require maki/cad.f
require maki/backward.f
require maki/gradcheck.f

package MAKI

: MBT-SLOT ( n -- MIR:input-slot )  MIR-SLOT-ID ;
: MBT-HAS-GRAD? ( n -- bool )  MBT-SLOT BW-HAS-GRAD? ;
: MBT-GRAD-NODE ( n -- CAD-KIND:node-id )
   MBT-SLOT BW-SLOT-GRAD@ MIR-REF-NODE ;
: MBT-GRAD-OP@ ( n -- n )  MBT-GRAD-NODE MIR-OP@ ;
: MBT-GRAD-ROWS@ ( n -- CAD-KIND:rows )  MBT-GRAD-NODE MIR-ROWS@ ;
: MBT-GRAD-COLS@ ( n -- CAD-KIND:cols )  MBT-GRAD-NODE MIR-COLS@ ;

: MLPT-REASON-IN ( ptr u8 n -- )  GC-RE$ 2swap CONTAINS? TTRUE ;

T-RESET

\ ---- the flagship MLP: x -> LINEAR -> GELU -> LINEAR -> y --------------------
\ forward IR: node0 LINEAR(x,w1,b1) 2x4 ; node1 GELU 2x4 ; node2 LINEAR(node1,w2,b2) 2x2
MODEL: MLP ( x:2x3 w1:3x4 b1:1x4 w2:4x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
MIR-N@ 3 T=
BW-CAN? TTRUE
BW-BUILD

\ every model input received a gradient: x + the four parameters (w1,b1,w2,b2)
0 MBT-HAS-GRAD? TTRUE  1 MBT-HAS-GRAD? TTRUE  2 MBT-HAS-GRAD? TTRUE
3 MBT-HAS-GRAD? TTRUE  4 MBT-HAS-GRAD? TTRUE

\ ---- parameter-gradient shape assertions -----------------------------------
\ d-x  = ct0 @ w1^T -> 2x3 (a transposed-matmul node)
0 MBT-GRAD-OP@ OP-MATMUL T=
0 MBT-GRAD-ROWS@ 2 ROWS-IS? TTRUE  0 MBT-GRAD-COLS@ 3 COLS-IS? TTRUE
\ d-w1 = x^T @ ct0 -> 3x4 (a transposed-matmul node)
1 MBT-GRAD-OP@ OP-MATMUL T=
1 MBT-GRAD-ROWS@ 3 ROWS-IS? TTRUE  1 MBT-GRAD-COLS@ 4 COLS-IS? TTRUE
\ d-b1 = row-reduce of ct0 -> 1x4 (OP-ROWSUM-BWD)
2 MBT-GRAD-OP@ OP-ROWSUM-BWD T=
2 MBT-GRAD-ROWS@ 1 ROWS-IS? TTRUE  2 MBT-GRAD-COLS@ 4 COLS-IS? TTRUE
\ d-w2 = node1^T @ ct2 -> 4x2 ; d-b2 = row-reduce of ct2 -> 1x2
3 MBT-GRAD-ROWS@ 4 ROWS-IS? TTRUE  3 MBT-GRAD-COLS@ 2 COLS-IS? TTRUE
4 MBT-GRAD-OP@ OP-ROWSUM-BWD T=
4 MBT-GRAD-ROWS@ 1 ROWS-IS? TTRUE  4 MBT-GRAD-COLS@ 2 COLS-IS? TTRUE

\ ---- gradcheck now RUNS on host: linear is host-executable (cad-7a executor) --
MODEL: MLP2 ( x:2x3 w1:3x4 b1:1x4 w2:4x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
GC-RUN V-PASS T=
s" host: 5 input(s) gradchecked" MLPT-REASON-IN
\ the gate wiring agrees: a real host pass
REPORT:NEW GRADCHECK-INTO G-GRADCHECK REPORT:GATE-TAG@ V-PASS T=

\ ---- the other reduce/scatter models gradcheck V-PASS on host too -----------
MODEL: MB ( x:2x3 b:1x3 -- y ) BIAS ;
GC-RUN V-PASS T=
MODEL: MSC ( x:2x3 s:1x1 -- z ) SCALE ;
GC-RUN V-PASS T=
MODEL: MG ( x:4x2 idx:3x1 -- y ) GATHER ;
GC-RUN V-PASS T=

T-REPORT

;package
