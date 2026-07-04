\ maki/regions-test.f - checked tests for elementwise region-candidate discovery.
\ Chains from captured models, no-chain (matmul breaks / lone EW), single-use
\ gating (a multi-use producer blocks fusion), report rows, and fail-closed idx.

require lib/test.f
require lib/string.f
require maki/cad.f
require maki/regions.f

package MAKI

variable GT-VA  variable GT-VU
: GT-SAVE ( ptr u8 n -- )  GT-VU ! GT-VA ! ;
: GT-IN ( ptr u8 n -- )  GT-VA @ GT-VU @ 2swap CONTAINS? TTRUE ;

: TRY-RGN-IDX ( -- )  99 RGN-START@ drop ;

T-RESET

\ ---- a pure elementwise chain: GELU SILU RELU -> one candidate [0..2] --------
MODEL: CHAIN ( x:4x8 -- y ) GELU SILU RELU ;
RGN-BUILD
RGN-COUNT     1 T=
0 RGN-START@  0 T=
0 RGN-END@    2 T=
0 RGN-LEN@    3 T=

\ report row for the candidate
RPT-NEW RGN-REPORT+ RPT-RENDER GT-SAVE
s" region-candidate: nodes 0..2 len 3 elementwise" GT-IN

\ ---- FFN: GELU is a lone elementwise node between two matmuls -> no candidate -
MODEL: FFN ( x:2x3 w1:3x4 b1:1x4 w2:4x5 b2:1x5 -- y ) LINEAR GELU LINEAR ;
RGN-BUILD
RGN-COUNT 0 T=

\ ---- MIX: GELU SILU (chain) then MATMUL (breaks) then RELU (lone) -----------
MODEL: MIX ( x:2x2 w:2x2 -- y ) GELU SILU MATMUL RELU ;
RGN-BUILD
RGN-COUNT     1 T=
0 RGN-START@  0 T=
0 RGN-END@    1 T=
0 RGN-LEN@    2 T=

\ ---- a movement node BREAKS an elementwise chain ---------------------------
\ GELU (node0) RESHAPE (node1, movement) RELU (node2): no chain spans the break.
MODEL: MVB ( x:4x8 -- y ) GELU RESHAPE:8x4 RELU ;
RGN-BUILD
RGN-COUNT 0 T=

\ ---- single-use gate: a multi-use producer blocks the chain ----------------
\ node0 = GELU(i0) ; node1 = ADD(n0, n0) -> n0 used twice -> not single-use.
MIR-RESET
0 0 DT-F32 LAY-ROW MIR-INPUT+ drop
OP-GELU MIR-OP-BEGIN  0 MIR-IN-REF MIR-IN+  0 0 DT-F32 LAY-ROW 0 1 MIR-OP+ drop
OP-ADD  MIR-OP-BEGIN  0 MIR-IN+ 0 MIR-IN+  0 0 DT-F32 LAY-ROW 0 1 MIR-OP+ drop
RGN-BUILD
RGN-COUNT 0 T=

\ ---- fail closed on a bad candidate index ----------------------------------
' TRY-RGN-IDX E-RGN-IDX TTHROWS

T-REPORT

end-package
