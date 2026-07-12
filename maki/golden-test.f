\ maki/golden-test.f - checked tests for the host GOLDEN self-consistency oracle.
\ Reference-complete + host-executable models are self-consistent (V-PASS); a cast op
\ (no host reference) is honest not-run with a named reason; the GOLDEN-INTO gate wiring
\ produces a real verdict.

require lib/test.f
require lib/string.f
require maki/cad.f
require maki/golden.f

package MAKI

: GOT-REASON-IN ( ptr u8 n -- )  GO-RE$ 2swap CONTAINS? TTRUE ;

T-RESET

\ ---- the FFN: reference-complete + host-executable -> self-consistent -------
MODEL: GO-FFN ( x:2x3 w1:3x4 b1:1x4 w2:4x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
GO-SUPPORTED? TTRUE
GO-RUN V-PASS T=
s" host self-consistent" GOT-REASON-IN

\ ---- single-op + movement + gather models are self-consistent too -----------
MODEL: GO-G ( x:2x2 -- y ) GELU ;
GO-RUN V-PASS T=
MODEL: GO-MM ( x:2x3 w:3x4 -- y ) MATMUL ;
GO-RUN V-PASS T=
MODEL: GO-LN ( x:4x8 -- y ) LAYERNORM ;
GO-RUN V-PASS T=
MODEL: GO-GA ( x:4x2 idx:3x1 -- y ) GATHER ;
GO-RUN V-PASS T=

\ ---- cast is not reference-complete -> not supported -> honest not-run ------
MODEL: GO-CAST ( x:2x2 -- y ) CAST ;
GO-SUPPORTED? TFALSE
GO-RUN V-NOTRUN T=
s" golden: incomplete op cast" GOT-REASON-IN

\ ---- gate wiring: GOLDEN-INTO produces a real verdict -----------------------
MODEL: GO-FFN2 ( x:2x3 w1:3x4 b1:1x4 w2:4x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
REPORT:NEW GOLDEN-INTO G-GOLDEN REPORT:GATE-TAG@ V-PASS T=
MODEL: GO-CAST2 ( x:2x2 -- y ) CAST ;
REPORT:NEW GOLDEN-INTO G-GOLDEN REPORT:GATE-TAG@ V-NOTRUN T=

T-REPORT

;package
