\ maki/mlp-bwd-test.f - the cad-9e MLP backward end-to-end proof.
\
\ The flagship LINEAR GELU LINEAR model builds a full backward IR (matmul adjoints +
\ gelu-bwd + the two bias row-reduces), with every parameter-gradient SHAPE asserted -
\ this is the capability habu-maki-from-scratch depends on (an MLP needs the linear
\ adjoint). Gradcheck returns an HONEST verdict: not-run, because LINEAR is not host-
\ executable (the reduce/scatter/matmul host executor is the cad-7 full-tensor path) -
\ NEVER a false pass. The other reduce/scatter models are likewise honest not-run.

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
0 BW-HAS-GRAD? TTRUE  1 BW-HAS-GRAD? TTRUE  2 BW-HAS-GRAD? TTRUE
3 BW-HAS-GRAD? TTRUE  4 BW-HAS-GRAD? TTRUE

\ ---- parameter-gradient shape assertions -----------------------------------
\ d-x  = ct0 @ w1^T -> 2x3 (a transposed-matmul node)
0 BW-SLOT-GRAD@ MIR-OP@   OP-MATMUL T=
0 BW-SLOT-GRAD@ MIR-ROWS@ 2 T=  0 BW-SLOT-GRAD@ MIR-COLS@ 3 T=
\ d-w1 = x^T @ ct0 -> 3x4 (a transposed-matmul node)
1 BW-SLOT-GRAD@ MIR-OP@   OP-MATMUL T=
1 BW-SLOT-GRAD@ MIR-ROWS@ 3 T=  1 BW-SLOT-GRAD@ MIR-COLS@ 4 T=
\ d-b1 = row-reduce of ct0 -> 1x4 (OP-ROWSUM-BWD)
2 BW-SLOT-GRAD@ MIR-OP@   OP-ROWSUM-BWD T=
2 BW-SLOT-GRAD@ MIR-ROWS@ 1 T=  2 BW-SLOT-GRAD@ MIR-COLS@ 4 T=
\ d-w2 = node1^T @ ct2 -> 4x2 ; d-b2 = row-reduce of ct2 -> 1x2
3 BW-SLOT-GRAD@ MIR-ROWS@ 4 T=  3 BW-SLOT-GRAD@ MIR-COLS@ 2 T=
4 BW-SLOT-GRAD@ MIR-OP@   OP-ROWSUM-BWD T=
4 BW-SLOT-GRAD@ MIR-ROWS@ 1 T=  4 BW-SLOT-GRAD@ MIR-COLS@ 2 T=

\ ---- gradcheck is HONEST not-run: linear is not host-executable (cad-7 path) --
MODEL: MLP2 ( x:2x3 w1:3x4 b1:1x4 w2:4x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
GC-RUN V-NOTRUN T=
s" host-unsupported:linear" MLPT-REASON-IN
\ the gate wiring agrees: never a false pass
RPT-NEW GRADCHECK-INTO G-GRADCHECK RPT-GATE-TAG@ V-NOTRUN T=

\ ---- the other reduce/scatter models are likewise honest not-run ------------
MODEL: MB ( x:2x3 b:1x3 -- y ) BIAS ;
GC-RUN V-NOTRUN T=  s" host-unsupported:bias" MLPT-REASON-IN
MODEL: MSC ( x:2x3 s:1x1 -- z ) SCALE ;
GC-RUN V-NOTRUN T=  s" host-unsupported:scale" MLPT-REASON-IN
MODEL: MG ( x:4x2 idx:3x1 -- y ) GATHER ;
GC-RUN V-NOTRUN T=  s" host-unsupported:gather" MLPT-REASON-IN

T-REPORT

end-package
