\ maki/traffic-test.f - checked tests for the cad-2 traffic (global-bytes) estimate.
\ dtype widths, before/after totals on a fused chain, the same-region broadcast
\ discount, unbound-extent honesty (bytes stay unknown + a named warning), and the
\ gathered read warning. Bytes = elements * dtype-width; f32 is 4 bytes.

require lib/test.f
require lib/string.f
require maki/cad.f
require maki/fusion-plan.f
require maki/traffic.f

package MAKI

variable TT-VA  variable TT-VU
: TT-SAVE ( ptr u8 n -- )  TT-VU ! TT-VA ! ;
: TT-IN ( ptr u8 n -- )  TT-VA @ TT-VU @ 2swap CONTAINS? TTRUE ;

T-RESET

\ (dtype byte widths are tensor.f DT-SIZE, covered by maki/tensor-test.f; the
\ old numeric DT-BYTES duplicate and its runtime bad-tag throw are retired)

\ ---- fused elementwise pair: GELU RELU on 2x4 (f32) ------------------------
\ before: 2 nodes * (read 8 + write 8) * 4B = 128; after: read x(8) + write y(8) = 64.
MODEL: EW2 ( x:2x4 -- y ) GELU RELU ;
FP-BUILD
TRF-BOUND? TTRUE
TRF-BEFORE 128 T=
TRF-AFTER   64 T=

\ ---- broadcast discount: an input read by two nodes IN ONE region counts once -
\ node0 = ADD(x, b) ; node1 = ADD(n0, b) -> b (slot1) read twice, discounted once.
MIR-RESET
2 4 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
2 4 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
OP-ADD MIR-OP-BEGIN 0 MIR-IN-REF MIR-IN+ 1 MIR-IN-REF MIR-IN+ 2 4 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
OP-ADD MIR-OP-BEGIN 0 MIR-IN+ 1 MIR-IN-REF MIR-IN+ 2 4 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
FP-BUILD
FP-REGION-COUNT 1 T=
TRF-BEFORE 192 T=                          \ 48 elems * 4
TRF-AFTER   96 T=                          \ x(8)+b(8) read once + y(8) write = 24 * 4

\ ---- unbound extent: bytes stay unknown, a warning names the input ----------
MODEL: UB ( x:0x8 -- y ) GELU ;
FP-BUILD
TRF-BOUND? TFALSE
REPORT:NEW TRF-INTO
dup REPORT:BYTES-KNOWN? TFALSE
REPORT:RENDER TT-SAVE
s" traffic.unbound: input 0" TT-IN

\ ---- gathered read: bytes known but flagged with a gathered warning ---------
MODEL: MGAT ( x:4x8 idx:3x1 -- y ) TRANSPOSE GATHER ;
FP-BUILD
TRF-BOUND? TTRUE
REPORT:NEW TRF-INTO
dup REPORT:BYTES-KNOWN? TTRUE
REPORT:RENDER TT-SAVE
s" traffic.gathered: node 1 gather" TT-IN

T-REPORT

end-package
