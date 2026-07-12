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

: MLPT-REASON-IN ( ptr u8 n -- )  GC-RE$ 2swap CONTAINS? TTRUE ;

T-RESET

\ ---- the flagship MLP: x -> LINEAR -> GELU -> LINEAR -> y --------------------
\ forward IR: node0 LINEAR(x,w1,b1) 2x4 ; node1 GELU 2x4 ; node2 LINEAR(node1,w2,b2) 2x2
MODEL: MLP ( x:2x3 w1:3x4 b1:1x4 w2:4x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
MIR-N@ 3 T=
BW-CAN? TTRUE
BW-BUILD

\ every model input received a gradient: x + the four parameters (w1,b1,w2,b2)
0 MIR-SLOT-ID BW-HAS-GRAD? TTRUE  1 MIR-SLOT-ID BW-HAS-GRAD? TTRUE  2 MIR-SLOT-ID BW-HAS-GRAD? TTRUE
3 MIR-SLOT-ID BW-HAS-GRAD? TTRUE  4 MIR-SLOT-ID BW-HAS-GRAD? TTRUE

\ ---- parameter-gradient shape assertions -----------------------------------
\ d-x  = ct0 @ w1^T -> 2x3 (a transposed-matmul node)
0 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE MIR-OP@   OPKIND>N OP-MATMUL T=
0 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE MIR-ROWS@ ROWS-RAW 2 T=  0 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE MIR-COLS@ COLS-RAW 3 T=
\ d-w1 = x^T @ ct0 -> 3x4 (a transposed-matmul node)
1 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE MIR-OP@   OPKIND>N OP-MATMUL T=
1 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE MIR-ROWS@ ROWS-RAW 3 T=  1 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE MIR-COLS@ COLS-RAW 4 T=
\ d-b1 = row-reduce of ct0 -> 1x4 (OP-ROWSUM-BWD)
2 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE MIR-OP@   OPKIND>N OP-ROWSUM-BWD T=
2 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE MIR-ROWS@ ROWS-RAW 1 T=  2 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE MIR-COLS@ COLS-RAW 4 T=
\ d-w2 = node1^T @ ct2 -> 4x2 ; d-b2 = row-reduce of ct2 -> 1x2
3 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE MIR-ROWS@ ROWS-RAW 4 T=  3 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE MIR-COLS@ COLS-RAW 2 T=
4 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE MIR-OP@   OPKIND>N OP-ROWSUM-BWD T=
4 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE MIR-ROWS@ ROWS-RAW 1 T=  4 MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE MIR-COLS@ COLS-RAW 2 T=

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
