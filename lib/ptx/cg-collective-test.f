\ cg-collective-test.f - the block-reduction EMIT shape (lib/ptx/cg-collective.f).
\
\ EMIT-REDUCE lowers BLOCK-MAX / BLOCK-SUM to a two-level WARP reduction: a
\ full-warp shfl.sync.down tree per 32-lane warp, per-warp partials staged to
\ shared, then a final single-warp reduce - NOT the old O(B) thread-0 fold. This
\ pins that shape off-device (the numeric proof is on the Orin:
\ tools/ptx/softmax-launch.f + sum-launch.f), and pins the inactive-lane identity
\ seed (-inf for max, 0 for sum) that must thread through BOTH shuffle levels so
\ inactive lanes (tid>=%r1) and past-the-warp-count final lanes contribute the
\ reducer identity, never garbage. In-process PTX capture; no device, no ptxas.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/cg-collective.f

256 %BLOCK

variable CGCT-A  variable CGCT-U
: CGCT-SAVE ( ptr u8 n -- )  CGCT-U ! CGCT-A ! ;
: CGCT$ ( -- ptr u8 n )  CGCT-A @ CGCT-U @ ;
: CGCT-IN     ( ptr u8 n -- )  CGCT$ 2swap CONTAINS? TTRUE ;        \ needle present
: CGCT-ABSENT ( ptr u8 n -- )  CGCT$ 2swap CONTAINS? 0= TTRUE ;     \ needle absent

\ capture EMIT-BLOCK-MAX / EMIT-BLOCK-SUM over a synthetic tile register (%f1),
\ after CG-SM-RESET seeds the softmax register counters.
: CGCT-CAP-MAX ( -- )  CG-SM-RESET  PTX-CAPTURE-ON  1 EMIT-BLOCK-MAX drop  PTX-CAPTURE-OFF  PTX-CAPTURE$ CGCT-SAVE ;
: CGCT-CAP-SUM ( -- )  CG-SM-RESET  PTX-CAPTURE-ON  1 EMIT-BLOCK-SUM drop  PTX-CAPTURE-OFF  PTX-CAPTURE$ CGCT-SAVE ;

T-RESET

\ ---- BLOCK-MAX: warp-shfl reduction, -inf inactive-lane identity -----------------
CGCT-CAP-MAX
s" shfl.sync.down.b32"  CGCT-IN         \ the warp-level tree step (the new path)
s" , 31, -1;"           CGCT-IN         \ full 32-lane clamp + membermask
s" max.f32"             CGCT-IN         \ max combine
s" 0fFF800000"          CGCT-IN         \ inactive-lane / past-count identity = -inf
s" st.shared.f32"       CGCT-IN         \ per-warp partial staged to shared
s" ld.shared.f32"       CGCT-IN         \ final reduce loads the partials + broadcast
s" bar.sync 0;"         CGCT-IN         \ stage + broadcast barriers
s" setp.lt.u32"         CGCT-IN         \ active-lane mask (tid<%r1) + warp-count mask
s" , 256;"              CGCT-ABSENT     \ NO O(B) block-size fold bound (regression)
s" add.f32"             CGCT-ABSENT     \ a max reduce never sums
s" 0f00000000"          CGCT-ABSENT     \ nor uses the sum identity 0

\ ---- BLOCK-SUM: same shape, 0 inactive-lane identity, add combine ---------------
CGCT-CAP-SUM
s" shfl.sync.down.b32"  CGCT-IN
s" , 31, -1;"           CGCT-IN
s" add.f32"             CGCT-IN         \ add combine
s" 0f00000000"          CGCT-IN         \ inactive-lane identity = 0
s" st.shared.f32"       CGCT-IN
s" bar.sync 0;"         CGCT-IN
s" , 256;"              CGCT-ABSENT     \ no O(B) fold
s" max.f32"             CGCT-ABSENT     \ a sum reduce never maxes
s" 0fFF800000"          CGCT-ABSENT     \ nor uses the -inf identity

\ ---- RMSNorm scale ops: count / eps / sqrt shapes -------------------------------
CG-SM-RESET  PTX-CAPTURE-ON  EMIT-UN drop  PTX-CAPTURE-OFF  PTX-CAPTURE$ CGCT-SAVE
s" cvt.rn.f32.u32"      CGCT-IN         \ mean denominator n = %r1 as f32
s" , %r1;"              CGCT-IN
CG-SM-RESET  PTX-CAPTURE-ON  1 EMIT-RMS-EPS+ drop  PTX-CAPTURE-OFF  PTX-CAPTURE$ CGCT-SAVE
s" add.f32"             CGCT-IN         \ eps added BEFORE the sqrt
s" 0f3727C5AC"          CGCT-IN         \ eps = F64>F32(1e-5) = maki RMS-EPS
CG-SM-RESET  PTX-CAPTURE-ON  1 EMIT-USQRT drop  PTX-CAPTURE-OFF  PTX-CAPTURE$ CGCT-SAVE
s" sqrt.rn.f32"         CGCT-IN

\ ---- RoPE pair rotation: partner shuffle + parity select ------------------------
CG-SM-RESET  PTX-CAPTURE-ON  1 2 3 EMIT-ROPE-ROT drop  PTX-CAPTURE-OFF  PTX-CAPTURE$ CGCT-SAVE
s" shfl.sync.bfly.b32"  CGCT-IN         \ partner coordinate = lane tid^1
s" , 1, 0x1f, -1;"      CGCT-IN         \ xor-1 partner, full-warp segment + membermask
s" and.b32"             CGCT-IN         \ lane parity (even/odd of the pair)
s" selp.f32"            CGCT-IN         \ even -> xc-ps, odd -> xc+ps
s" bar.sync"            CGCT-ABSENT     \ warp-scoped rotation: NO block barrier

T-REPORT
