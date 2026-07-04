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

: TRY-DT ( -- )  99 DT-BYTES drop ;                       \ unknown dtype fails closed

T-RESET

\ ---- dtype byte widths -----------------------------------------------------
DT-F32  DT-BYTES 4 T=
DT-F16  DT-BYTES 2 T=
DT-BF16 DT-BYTES 2 T=
DT-U32  DT-BYTES 4 T=
DT-I32  DT-BYTES 4 T=
' TRY-DT E-MK-DTYPE TTHROWS

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
2 4 DT-F32 LAY-ROW MIR-INPUT+ drop
2 4 DT-F32 LAY-ROW MIR-INPUT+ drop
OP-ADD MIR-OP-BEGIN 0 MIR-IN-REF MIR-IN+ 1 MIR-IN-REF MIR-IN+ 2 4 DT-F32 LAY-ROW 0 1 MIR-OP+ drop
OP-ADD MIR-OP-BEGIN 0 MIR-IN+ 1 MIR-IN-REF MIR-IN+ 2 4 DT-F32 LAY-ROW 0 1 MIR-OP+ drop
FP-BUILD
FP-REGION-COUNT 1 T=
TRF-BEFORE 192 T=                          \ 48 elems * 4
TRF-AFTER   96 T=                          \ x(8)+b(8) read once + y(8) write = 24 * 4

\ ---- unbound extent: bytes stay unknown, a warning names the input ----------
MODEL: UB ( x:0x8 -- y ) GELU ;
FP-BUILD
TRF-BOUND? TFALSE
RPT-NEW TRF-INTO
dup RPT-BYTES-KNOWN? TFALSE
RPT-RENDER TT-SAVE
s" traffic.unbound: input 0" TT-IN

\ ---- gathered read: bytes known but flagged with a gathered warning ---------
MODEL: MGAT ( x:4x8 idx:3x1 -- y ) TRANSPOSE GATHER ;
FP-BUILD
TRF-BOUND? TTRUE
RPT-NEW TRF-INTO
dup RPT-BYTES-KNOWN? TTRUE
RPT-RENDER TT-SAVE
s" traffic.gathered: node 1 gather" TT-IN

T-REPORT

end-package
