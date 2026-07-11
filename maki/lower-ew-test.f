\ maki/lower-ew-test.f - host-side checked tests for the elementwise region lowering.
\
\ Off-device: builds captured models, lowers a region to PTX text (in-process capture,
\ src/arch/ptx/emit.f PTX-CAPTURE-ON), and asserts the flat-kernel shape - exactly one
\ .version header, exactly one REGION_<rid> entry, the per-input/out/n params, and the
\ expected op instructions for a GELU->RELU chain and a two-input ADD->RELU chain. Then the
\ broadcast operand lowerings: a BIAS 1xC row-broadcast (rem.u32 mod-C remap, add.rn) and a
\ SCALE 1x1 scalar-broadcast (mov.u64 zero offset, mul.rn, no rem/div), mirroring EX-BC@. Then
\ the fail-closed paths: non-elementwise region, unsupported op, the >4-input cap, and an
\ illegal broadcast shape (a dim neither 1 nor full). No device, no ptxas - PTX validity is
\ proven on the Orin by maki/lower-device-test.f. Load via maki/test.f.

require lib/test.f
require lib/string.f
require maki/cad.f
require maki/lower-ew.f

package MAKI

\ ---- captured-text assertions (LEWT- prefix: shares the dictionary with siblings) --
variable LEWT-VA  variable LEWT-VU
: LEWT-SAVE ( ptr u8 n -- )  LEWT-VU ! LEWT-VA ! ;
: LEWT$ ( -- ptr u8 n )  LEWT-VA @ LEWT-VU @ ;
: LEWT-IN     ( ptr u8 n -- )  LEWT$ 2swap CONTAINS? TTRUE ;      \ LEWT$ contains the needle
: LEWT-ABSENT ( ptr u8 n -- )  LEWT$ 2swap FIND-SUB 0 < TTRUE ;   \ needle absent

\ exactly-one: the needle occurs once (find it, then prove no second occurrence)
: LEWT-ONCE? ( ptr u8 n ptr u8 n -- bool ) {: ha:ptr hu:n na:ptr nu:n :}
   ha hu na nu FIND-SUB {: i1:n :}
   i1 0 < if false exit then
   ha i1 + nu +  hu i1 - nu -  na nu FIND-SUB 0 < ;
: LEWT-ONCE ( ptr u8 n -- )  LEWT$ 2swap LEWT-ONCE? TTRUE ;

: LEWT-CAP0 ( -- )  PTX-CAPTURE-ON  0 LEW-EMIT  PTX-CAPTURE-OFF  PTX-CAPTURE$ LEWT-SAVE ;

\ ---- fail-closed probes (each acts on the current model) --------------------
: LEWT-TRY-NOTEW  ( -- )  0 LEW-EMIT ;      \ non-elementwise region -> reject before emit
: LEWT-TRY-OP     ( -- )  0 LEW-ANALYZE ;   \ unsupported elementwise op
: LEWT-TRY-INPUTS ( -- )  0 LEW-ANALYZE ;   \ more than the v1 input cap
: LEWT-TRY-BCAST  ( -- )  0 LEW-ANALYZE ;   \ illegal broadcast shape (a dim neither 1 nor full)
: LEWT-TRY-MOUT   ( -- )  0 LEW-ANALYZE ;   \ >1 materialized output in the region (corrupted plan)

T-RESET

\ ---- GELU RELU: one flat kernel, gelu (ex2.approx) then relu (max.f32) -------
MODEL: GR ( x:4x8 -- y ) GELU RELU ;
FP-BUILD
LEWT-CAP0
s" .version 8.3"              LEWT-ONCE
s" .visible .entry REGION_0"  LEWT-ONCE
s" .param .u64 p_in0"         LEWT-IN
s" .param .u64 p_out"         LEWT-IN
s" .param .u32 p_n"           LEWT-IN
s" ld.param.u32 %r1, [p_n];"  LEWT-IN
s" ex2.approx.f32"            LEWT-IN
s" max.f32"                   LEWT-IN
s" DONE:"                     LEWT-IN
s" ret;"                      LEWT-IN

\ ---- two-input ADD RELU: two params, a binary add.rn.f32 --------------------
MODEL: AR ( a:2x4 b:2x4 -- y ) ADD RELU ;
FP-BUILD
LEWT-CAP0
s" .visible .entry REGION_0"  LEWT-ONCE
s" .param .u64 p_in0"         LEWT-IN
s" .param .u64 p_in1"         LEWT-IN
s" add.rn.f32"                LEWT-IN
s" max.f32"                   LEWT-IN

\ ---- BIAS 4x8 + 1x8: a 1xC row-broadcast param loads [e mod C] (rem.u32), add.rn ----
\ The bias operand is a second kernel input whose flat load index is remapped to (e mod C=8),
\ mirroring the host executor EX-BC@ 1xC read; BIAS lowers as add.rn.f32. No div (not Rx1).
MODEL: MB ( x:4x8 b:1x8 -- y ) BIAS ;
FP-BUILD
LEWT-CAP0
s" .version 8.3"              LEWT-ONCE
s" .visible .entry REGION_0"  LEWT-ONCE
s" .param .u64 p_in1"         LEWT-IN
s" rem.u32"                   LEWT-IN       \ 1xC remap: flat e mod C
s" , 8;"                      LEWT-IN       \ ...by the C=8 immediate
s" add.rn.f32"                LEWT-IN       \ BIAS lowers as add
s" div.u32"                   LEWT-ABSENT   \ a 1xC row-broadcast is not a col-broadcast

\ ---- SCALE 4x8 + 1x1: a 1x1 scalar-broadcast param loads [0] (mov.u64 0), mul.rn ----
\ The scale operand reads element 0 for every lane via a zero byte offset, mirroring EX-BC@
\ 1x1; SCALE lowers as mul.rn.f32. Mod/div-free (no rem/div: the scalar has no index math).
MODEL: MS ( x:4x8 s:1x1 -- y ) SCALE ;
FP-BUILD
LEWT-CAP0
s" .version 8.3"              LEWT-ONCE
s" .visible .entry REGION_0"  LEWT-ONCE
s" mov.u64"                   LEWT-IN       \ scalar zero byte offset
s" , 0;"                      LEWT-IN
s" mul.rn.f32"                LEWT-IN       \ SCALE lowers as mul
s" rem.u32"                   LEWT-ABSENT   \ scalar load is mod-free
s" div.u32"                   LEWT-ABSENT   \ scalar load is div-free

\ ---- fail closed: a row-reduction region is not elementwise -----------------
MODEL: LN ( x:4x8 -- y ) LAYERNORM ;
FP-BUILD
' LEWT-TRY-NOTEW E-LEW-NOTEW TTHROWS

\ ---- fail closed: CAST is elementwise but has no v1 device emitter ----------
MODEL: CS ( x:4x8 -- y ) CAST ;
FP-BUILD
' LEWT-TRY-OP E-LEW-OP TTHROWS

\ ---- fail closed: a 5-input add chain exceeds the v1 input cap (4) ----------
MODEL: A5 ( a:2x4 b:2x4 c:2x4 d:2x4 e:2x4 -- y ) ADD ADD ADD ADD ;
FP-BUILD
' LEWT-TRY-INPUTS E-LEW-INPUTS TTHROWS

\ ---- fail closed: an ILLEGAL broadcast shape (a dim neither 1 nor full) ------------
\ Legal broadcasts (1xC / 1x1 / Rx1) now lower; only a shape that is neither full nor a
\ unit dim is rejected. Capture-time shape legality (E-CAD-PARAM-SHAPE) rejects a mismatched
\ ADD at MODEL:, so the IR is hand-built (backward-test pattern) with a 3x8 operand into a
\ 4x8 region to keep LEW-ANALYZE's own guard tested as defense-in-depth (it must not trust
\ its caller). 3 is neither 1 nor R=4 -> BC-ILLEGAL -> E-LEW-BCAST.
MIR-RESET
4 8 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
3 8 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
MAKI-OPKIND:ADD MIR-OP-BEGIN  0 MIR-IN-REF MIR-IN+  1 MIR-IN-REF MIR-IN+
   4 8 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
FP-BUILD
' LEWT-TRY-BCAST E-LEW-BCAST TTHROWS

\ ---- fail closed: a corrupted plan with two materialized outputs in one region -----
\ The planner NEVER produces this (maki/fusion-mout-test.f proves each region has exactly
\ one materialized output), so E-LEW-MULTIOUT is a defense-in-depth invariant guard, not a
\ v1 feature cap. FP-BUILD leaves GELU interior (mat=0) and RELU the sole output (mat=1);
\ forcing GELU materialized simulates a bad plan LEW-ANALYZE (LEW-FIND-OUT) must reject.
MODEL: MO ( x:4x8 -- y ) GELU RELU ;
FP-BUILD
-1 0 MIR-MAT!
' LEWT-TRY-MOUT E-LEW-MULTIOUT TTHROWS

T-REPORT

end-package
