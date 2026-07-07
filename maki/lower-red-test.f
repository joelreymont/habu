\ maki/lower-red-test.f - host-side checked tests for the row-reduce region lowering.
\
\ Off-device: builds captured models, lowers a region to PTX text (in-process capture,
\ src/arch/ptx/emit.f PTX-CAPTURE-ON), and asserts the block-per-row kernel shape - one
\ .version header, one REGION_<rid> entry, the per-input/out/k params, the shared SMEM +
\ bar.sync block reduction, and the per-op signature instructions for RMSNORM (sqrt, no
\ subtract), LAYERNORM (subtract + sqrt), SOFTMAX-ROW (block-max + ex2.approx, no sqrt),
\ plus prologue fusion (GELU->RMSNORM in one kernel; ADD->RMSNORM with two params). Then the
\ broadcast prologues: BIAS->RMSNORM (a 1xC row-broadcast pinned to row 0, add.rn) and
\ SCALE->RMSNORM (a 1x1 scalar-broadcast via a zero column offset, mul.rn), mirroring EX-BC@.
\ Then the fail-closed paths: a pure-elementwise region, an unsupported op, two reductions, the
\ column cap, the input cap, an Rx1 column and an illegal broadcast shape (both hand-built IR),
\ and a corrupted multi-output plan. No device, no ptxas - PTX validity is proven on the Orin by
\ maki/lower-device-test.f. Load via maki/test.f.

require lib/test.f
require lib/string.f
require maki/cad.f
require maki/lower-red.f

package MAKI

\ ---- captured-text assertions (LREDT- prefix: shares the dictionary with siblings) --
variable LREDT-VA  variable LREDT-VU
: LREDT-SAVE ( ptr u8 n -- )  LREDT-VU ! LREDT-VA ! ;
: LREDT$ ( -- ptr u8 n )  LREDT-VA @ LREDT-VU @ ;
: LREDT-IN     ( ptr u8 n -- )  LREDT$ 2swap CONTAINS? TTRUE ;         \ needle present
: LREDT-ABSENT ( ptr u8 n -- )  LREDT$ 2swap FIND-SUB 0 < TTRUE ;      \ needle absent

\ exactly-one: the needle occurs once (find it, then prove no second occurrence)
: LREDT-ONCE? ( ptr u8 n ptr u8 n -- bool ) {: ha:ptr hu:n na:ptr nu:n :}
   ha hu na nu FIND-SUB {: i1:n :}
   i1 0 < if false exit then
   ha i1 + nu +  hu i1 - nu -  na nu FIND-SUB 0 < ;
: LREDT-ONCE ( ptr u8 n -- )  LREDT$ 2swap LREDT-ONCE? TTRUE ;

: LREDT-CAP0 ( -- )  PTX-CAPTURE-ON  0 LRED-EMIT  PTX-CAPTURE-OFF  PTX-CAPTURE$ LREDT-SAVE ;

\ ---- fail-closed probes (each acts on the current model / plan) --------------
: LREDT-TRY-EMIT ( -- )  0 LRED-EMIT ;      \ non-reduction region -> reject before emit
: LREDT-TRY      ( -- )  0 LRED-ANALYZE ;   \ analysis-only fail-closed checks

T-RESET

\ ---- RMSNORM 4x8: one block-per-row kernel, sum-of-squares reduction + scale ----
MODEL: RM ( x:4x8 -- y ) RMSNORM ;
FP-BUILD
LREDT-CAP0
s" .version 8.3"                LREDT-ONCE
s" .visible .entry REGION_0"    LREDT-ONCE
s" .param .u64 p_in0"           LREDT-IN
s" .param .u64 p_out"           LREDT-IN
s" .param .u32 p_k"             LREDT-IN
s" ld.param.u32 %r1, [p_k];"    LREDT-IN
s" .shared .align 4 .b8 SMEM["  LREDT-IN
s" bar.sync 0;"                 LREDT-IN
s" add.f32"                     LREDT-IN     \ block-sum fold (+ eps add)
s" cvt.rn.f32.u32"              LREDT-IN     \ mean = sum / k
s" sqrt.rn.f32"                 LREDT-IN     \ r = sqrt(ms + eps)
s" div.rn.f32"                  LREDT-IN     \ y = x / r
s" ret;"                        LREDT-IN
s" sub.f32"                     LREDT-ABSENT \ rmsnorm subtracts no mean
s" ex2.approx"                  LREDT-ABSENT \ no exponential
s" max.f32"                     LREDT-ABSENT \ no block-max

\ ---- LAYERNORM 4x8: two block reductions (mean, var), subtract, normalize -------
MODEL: LN ( x:4x8 -- y ) LAYERNORM ;
FP-BUILD
LREDT-CAP0
s" .visible .entry REGION_0"    LREDT-ONCE
s" cvt.rn.f32.u32"              LREDT-IN     \ mean / var divide by k
s" sub.f32"                     LREDT-IN     \ x - mu (B-)
s" add.f32"                     LREDT-IN     \ block-sum fold
s" sqrt.rn.f32"                 LREDT-IN     \ std = sqrt(var + eps)
s" div.rn.f32"                  LREDT-IN     \ (x - mu) / std
s" ex2.approx"                  LREDT-ABSENT
s" max.f32"                     LREDT-ABSENT

\ ---- SOFTMAX-ROW 4x8: block-max, subtract, exp, block-sum, divide ---------------
MODEL: SM ( x:4x8 -- y ) SOFTMAX-ROW ;
FP-BUILD
LREDT-CAP0
s" .visible .entry REGION_0"    LREDT-ONCE
s" max.f32"                     LREDT-IN     \ block-max fold
s" sub.f32"                     LREDT-IN     \ x - max (B-)
s" ex2.approx.f32"              LREDT-IN     \ exp
s" add.f32"                     LREDT-IN     \ block-sum fold
s" div.rn.f32"                  LREDT-IN     \ e / sum (B/)
s" sqrt.rn.f32"                 LREDT-ABSENT \ softmax has no sqrt

\ ---- GELU->RMSNORM: leading elementwise prologue fused into ONE kernel ----------
MODEL: GR ( x:4x8 -- y ) GELU RMSNORM ;
FP-BUILD
LREDT-CAP0
s" .version 8.3"                LREDT-ONCE   \ still a single module
s" .visible .entry REGION_0"    LREDT-ONCE   \ still a single kernel
s" .param .u64 p_in0"           LREDT-IN
s" .param .u64 p_out"           LREDT-IN
s" ex2.approx.f32"              LREDT-IN     \ gelu prologue (tanh path)
s" sqrt.rn.f32"                 LREDT-IN     \ rmsnorm reduction

\ ---- ADD->RMSNORM: two-input elementwise prologue (two params, tile add) ---------
MODEL: AR ( a:4x8 b:4x8 -- y ) ADD RMSNORM ;
FP-BUILD
LREDT-CAP0
s" .visible .entry REGION_0"    LREDT-ONCE
s" .param .u64 p_in0"           LREDT-IN
s" .param .u64 p_in1"           LREDT-IN
s" add.rn.f32"                  LREDT-IN     \ tile+tile ADD prologue
s" sqrt.rn.f32"                 LREDT-IN     \ rmsnorm reduction

\ ---- BIAS->RMSNORM: a 1xC row-broadcast prologue pinned to row 0 (same row/block) -
\ The bias operand (p_in1) is loaded from row 0 with the shared column ctx (element tid),
\ so it needs no zero column offset (mov.u64 absent); BIAS lowers as add.rn.f32.
MODEL: BR ( x:4x8 b:1x8 -- y ) BIAS RMSNORM ;
FP-BUILD
LREDT-CAP0
s" .version 8.3"                LREDT-ONCE
s" .visible .entry REGION_0"    LREDT-ONCE
s" .param .u64 p_in1"           LREDT-IN
s" add.rn.f32"                  LREDT-IN     \ BIAS tile add
s" sqrt.rn.f32"                 LREDT-IN     \ rmsnorm reduction
s" mov.u64"                     LREDT-ABSENT \ 1xC uses the shared tid ctx, not a scalar zero offset

\ ---- SCALE->RMSNORM: a 1x1 scalar-broadcast prologue (row 0 + zero column offset) -
\ The scale operand reads element 0 for every lane via a zero byte offset (mov.u64 ..,0);
\ SCALE lowers as mul.rn.f32.
MODEL: SR ( x:4x8 s:1x1 -- y ) SCALE RMSNORM ;
FP-BUILD
LREDT-CAP0
s" .visible .entry REGION_0"    LREDT-ONCE
s" .param .u64 p_in1"           LREDT-IN
s" mov.u64"                     LREDT-IN     \ scalar zero column offset
s" , 0;"                        LREDT-IN
s" mul.rn.f32"                  LREDT-IN     \ SCALE tile mul
s" sqrt.rn.f32"                 LREDT-IN     \ rmsnorm reduction

\ ---- fail closed: a pure-elementwise region is not a reduction region ------------
MODEL: GRL ( x:4x8 -- y ) GELU RELU ;
FP-BUILD
' LREDT-TRY-EMIT E-LRED-NOTRED TTHROWS

\ ---- fail closed: CAST is elementwise but has no v1 device emitter ---------------
MODEL: CR ( x:4x8 -- y ) CAST RMSNORM ;
FP-BUILD
' LREDT-TRY E-LRED-OP TTHROWS

\ ---- fail closed: two chained reductions exceed the v1 single-reduction cap ------
MODEL: RS ( x:4x8 -- y ) RMSNORM SOFTMAX-ROW ;
FP-BUILD
' LREDT-TRY E-LRED-MULTIRED TTHROWS

\ ---- fail closed: a row wider than the block (k > 256) ---------------------------
MODEL: WC ( x:4x512 -- y ) RMSNORM ;
FP-BUILD
' LREDT-TRY E-LRED-COLS TTHROWS

\ ---- fail closed: a 5-input add prologue exceeds the v1 input cap (4) -------------
MODEL: A5 ( a:4x8 b:4x8 c:4x8 d:4x8 e:4x8 -- y ) ADD ADD ADD ADD RMSNORM ;
FP-BUILD
' LREDT-TRY E-LRED-INPUTS TTHROWS

\ ---- fail closed: a corrupted plan with two materialized outputs in one region ---
\ FP-BUILD leaves GELU interior (mat=0) and RMSNORM the sole output (mat=1). Forcing
\ GELU materialized simulates a bad plan; LRED-ANALYZE must not trust it.
MODEL: MO ( x:4x8 -- y ) GELU RMSNORM ;
FP-BUILD
-1 0 MIR-MAT!
' LREDT-TRY E-LRED-MULTIOUT TTHROWS

\ ---- fail closed: an Rx1 COLUMN broadcast into the reduction (no clean row load) ---
\ A 1xC row / 1x1 scalar broadcast now lowers (pinned to row 0); an Rx1 column would need a
\ stride-1 row span the coalesced row loader cannot express, so it stays fail-closed. It is
\ not a legal capture class either, so the IR is hand-built (backward-test pattern) to keep
\ LRED-ANALYZE's guard tested as defense-in-depth. 4x1 into 4x8: br=R=4, bc=1 -> BC-COL.
MIR-RESET
4 8 DT-F32 LAY-ROW MIR-INPUT+ drop
4 1 DT-F32 LAY-ROW MIR-INPUT+ drop
OP-ADD MIR-OP-BEGIN  0 MIR-IN-REF MIR-IN+  1 MIR-IN-REF MIR-IN+
   4 8 DT-F32 LAY-ROW 0 1 MIR-OP+ drop
OP-RMSNORM MIR-OP-BEGIN  0 MIR-IN+
   4 8 DT-F32 LAY-ROW 0 1 MIR-OP+ drop
FP-BUILD
' LREDT-TRY E-LRED-BCAST TTHROWS

\ ---- fail closed: an ILLEGAL broadcast shape (a dim neither 1 nor full) ------------
\ 3x8 into a 4x8 region: 3 is neither 1 nor R=4 -> BC-ILLEGAL -> E-LRED-BCAST.
MIR-RESET
4 8 DT-F32 LAY-ROW MIR-INPUT+ drop
3 8 DT-F32 LAY-ROW MIR-INPUT+ drop
OP-ADD MIR-OP-BEGIN  0 MIR-IN-REF MIR-IN+  1 MIR-IN-REF MIR-IN+
   4 8 DT-F32 LAY-ROW 0 1 MIR-OP+ drop
OP-RMSNORM MIR-OP-BEGIN  0 MIR-IN+
   4 8 DT-F32 LAY-ROW 0 1 MIR-OP+ drop
FP-BUILD
' LREDT-TRY E-LRED-BCAST TTHROWS

T-REPORT

end-package
