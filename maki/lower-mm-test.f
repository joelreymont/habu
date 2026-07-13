\ maki/lower-mm-test.f - host-side checked tests for the matmul/linear region lowering.
\
\ Off-device: builds captured models, lowers a region to PTX text (in-process capture,
\ src/arch/ptx/emit.f PTX-CAPTURE-ON), and asserts the tiled-GEMM kernel shape - one
\ .version header, one REGION_<rid> entry, the per-input/out params + M/N/K u32 params, the
\ runtime K-loop ($KLOOP/$KEND with fma.rn.f32), the LINEAR bias add, and the fused unary
\ epilogue (MATMUL->RELU max.f32; LINEAR->GELU ex2.approx). Then the fail-closed paths: a
\ non-matmul region, an unsupported epilogue op, a prologue EW into the contraction, K over
\ the accumulation cap, output dims over the arena cap, a corrupted multi-output plan, and a
\ corrupted two-contraction plan (hand-forced FP-RID, defense-in-depth). No device, no
\ ptxas - PTX validity is proven on the Orin by maki/lower-mm-device-test.f. Load via maki/test.f.

require lib/test.f
require lib/string.f
require maki/cad.f
require maki/lower-mm.f

package MAKI

\ ---- captured-text assertions (LMMT- prefix: shares the dictionary with siblings) --
variable LMMT-VA  variable LMMT-VU
: LMMT-SAVE ( ptr u8 n -- )  LMMT-VU ! LMMT-VA ! ;
: LMMT$ ( -- ptr u8 n )  LMMT-VA @ LMMT-VU @ ;
: LMMT-IN     ( ptr u8 n -- )  LMMT$ 2swap CONTAINS? TTRUE ;         \ needle present
: LMMT-ABSENT ( ptr u8 n -- )   \ needle absent
   LMMT$ 2swap FIND-SUB MATCH option
     none OF true ENDOF
     some OF drop false ENDOF
   ;MATCH TTRUE ;

\ exactly-one: the needle occurs once (find it, then prove no second occurrence)
: LMMT-ONCE? ( ptr u8 n ptr u8 n -- bool ) {: ha:ptr hu:n na:ptr nu:n :}
   ha hu na nu FIND-SUB MATCH option
     none OF false exit ENDOF
     some OF IDX>N ENDOF
   ;MATCH {: i1:n :}
   ha i1 + nu +  hu i1 - nu -  na nu FIND-SUB MATCH option
     none OF true ENDOF
     some OF drop false ENDOF
   ;MATCH ;
: LMMT-ONCE ( ptr u8 n -- )  LMMT$ 2swap LMMT-ONCE? TTRUE ;

: LMMT-CAP0 ( -- )  PTX-CAPTURE-ON  0 FP-REGION-ID LMM-EMIT  PTX-CAPTURE-OFF  PTX-CAPTURE$ LMMT-SAVE ;

\ ---- fail-closed probes (each acts on the current model / plan) --------------
: LMMT-TRY-EMIT ( -- )  0 FP-REGION-ID LMM-EMIT ;      \ non-matmul region -> reject before emit
: LMMT-TRY      ( -- )  0 FP-REGION-ID LMM-ANALYZE ;   \ analysis-only fail-closed checks (region 0)
: LMMT-TRY1     ( -- )  1 FP-REGION-ID LMM-ANALYZE ;   \ analysis-only fail-closed checks (region 1)

T-RESET

\ ---- MATMUL 8x8@8x8: pure contraction, no bias, no epilogue ----------------------
MODEL: MM ( x:8x8 w:8x8 -- y ) MATMUL ;
FP-BUILD
LMMT-CAP0
s" .version 8.3"                LMMT-ONCE
s" .visible .entry REGION_0"    LMMT-ONCE
s" .param .u64 p_in0"           LMMT-IN
s" .param .u64 p_in1"           LMMT-IN
s" .param .u64 p_out"           LMMT-IN
s" .param .u32 p_m"             LMMT-IN
s" .param .u32 p_n"             LMMT-IN
s" .param .u32 p_k"             LMMT-IN
s" $KLOOP:"                     LMMT-IN
s" $KEND:"                      LMMT-IN
s" fma.rn.f32"                  LMMT-IN     \ K-loop accumulate
s" st.global.f32"               LMMT-IN
s" ret;"                        LMMT-IN
s" .param .u64 p_in2"           LMMT-ABSENT \ matmul has two inputs (no bias)
s" add.rn.f32"                  LMMT-ABSENT \ no bias, no add epilogue
s" ex2.approx"                  LMMT-ABSENT \ no gelu
s" max.f32"                     LMMT-ABSENT \ no relu
s" .shared"                     LMMT-ABSENT \ naive tile: no shared staging

\ ---- LINEAR 4x8@8x16: bias operand + folded bias add, no epilogue ----------------
MODEL: LN ( x:4x8 w:8x16 b:1x16 -- y ) LINEAR ;
FP-BUILD
LMMT-CAP0
s" .visible .entry REGION_0"    LMMT-ONCE
s" .param .u64 p_in2"           LMMT-IN     \ the bias operand
s" fma.rn.f32"                  LMMT-IN
s" add.rn.f32"                  LMMT-IN     \ acc += bias[col]
s" ex2.approx"                  LMMT-ABSENT \ no epilogue activation
s" max.f32"                     LMMT-ABSENT
\ typed M/N sources agree with the staged raw dims (C(4x16) = A(4x8) . B(8x16));
\ K stays raw by design (cols(A) = rows(B): no honest single CAD-KIND role)
LMM-ROWS@ ROWS-RAW 4  T=  LMM-ROWS@ ROWS-RAW LMM-M@ T=
LMM-COLS@ COLS-RAW 16 T=  LMM-COLS@ COLS-RAW LMM-N@ T=
LMM-K@ 8 T=

\ ---- LINEAR GELU 4x8@8x16: bias then a fused gelu epilogue on the accumulator -----
MODEL: LG ( x:4x8 w:8x16 b:1x16 -- y ) LINEAR GELU ;
FP-BUILD
LMMT-CAP0
s" .version 8.3"                LMMT-ONCE   \ still a single module
s" .visible .entry REGION_0"    LMMT-ONCE   \ still a single kernel
s" .param .u64 p_in2"           LMMT-IN
s" add.rn.f32"                  LMMT-IN     \ bias add
s" ex2.approx.f32"              LMMT-IN     \ gelu (tanh path via sigmoid)
s" st.global.f32"               LMMT-IN

\ ---- MATMUL RELU 8x8@8x8: a fused relu epilogue (max.f32), no bias ---------------
MODEL: MR ( x:8x8 w:8x8 -- y ) MATMUL RELU ;
FP-BUILD
LMMT-CAP0
s" .visible .entry REGION_0"    LMMT-ONCE
s" fma.rn.f32"                  LMMT-IN
s" max.f32"                     LMMT-IN     \ relu epilogue
s" add.rn.f32"                  LMMT-ABSENT \ no bias (matmul, not linear)
s" ex2.approx"                  LMMT-ABSENT

\ ---- BLOCKED 64x64x64 MATMUL: register-blocked tile (shared staging, 4x4 accumulators) -----
\ M,N multiples of 64 and K a multiple of 32 (family floor) select the perf tile; block is 16x16.
MODEL: MB ( x:64x64 w:64x64 -- y ) MATMUL ;
FP-BUILD
LMMT-CAP0
LMM-BLOCKED?                    TTRUE       \ shape selects the register-blocked tile
LMM-OUT-TILE@ 64                T=          \ launch grid tiles the 64x64 output (not 16)
s" .version 8.3"                LMMT-ONCE
s" .visible .entry REGION_0"    LMMT-ONCE
s" .shared .align 16 .b8 SH["   LMMT-IN     \ 2x As/Bs staging, 16B-aligned (cp.async + v4)
s" bar.sync 0;"                 LMMT-IN     \ pipeline barrier
s" cp.async.cg.shared.global"   LMMT-IN     \ direct global->shared staging (no register round-trip)
s" cp.async.commit_group;"      LMMT-IN     \ double-buffer group commit
s" cp.async.wait_group 1;"      LMMT-IN     \ overlap: keep the prefetch group in flight
s" ld.shared.f32 %f26"          LMMT-IN     \ scalar A micro-tile loads (column-strided As)
s" ld.shared.v4.f32 {%f30,%f31,%f32,%f33}"  LMMT-IN  \ vectorized B load (4 contiguous cols)
s" $KLOOP:"                     LMMT-IN
s" fma.rn.f32"                  LMMT-IN     \ 4x4 outer-product accumulate
s" mov.f32 %f25,"               LMMT-IN     \ 16th accumulator (4x4 micro-tile = %f10..%f25)
s" st.global.f32"               LMMT-IN
s" st.shared.f32"               LMMT-ABSENT \ cp.async stages direct; no shared store
s" .param .u64 p_in2"           LMMT-ABSENT \ matmul: no bias
s" add.rn.f32"                  LMMT-ABSENT \ no bias, no epilogue add
s" ex2.approx"                  LMMT-ABSENT \ no gelu

\ ---- BLOCKED 64x64x64 LINEAR GELU: bias + gelu fused into each micro-tile store ------------
MODEL: LGB ( x:64x64 w:64x64 b:1x64 -- y ) LINEAR GELU ;
FP-BUILD
LMMT-CAP0
LMM-BLOCKED?                    TTRUE
LMM-OUT-TILE@ 64                T=
s" .shared .align 16 .b8 SH["   LMMT-IN
s" .param .u64 p_in2"           LMMT-IN     \ the bias operand
s" ld.shared.v4.f32"            LMMT-IN     \ vectorized B load in the blocked epilogue path
s" add.rn.f32"                  LMMT-IN     \ acc += bias[col] per micro-tile element
s" ex2.approx.f32"              LMMT-IN     \ gelu epilogue on each element
s" st.global.f32"               LMMT-IN

\ ---- fail closed: a pure row-reduce region is not a matmul region ----------------
MODEL: NR ( x:4x8 -- y ) RMSNORM ;
FP-BUILD
' LMMT-TRY-EMIT E-LMM-NOTMM TTHROWS

\ ---- fail closed: CAST is an EW epilogue with no v1 device emitter ----------------
MODEL: MC ( x:8x8 w:8x8 -- y ) MATMUL CAST ;
FP-BUILD
' LMMT-TRY E-LMM-OP TTHROWS

\ ---- fail closed: an EW op fused as a contraction PROLOGUE (hand-forced, defense-in-depth) ---
\ The backend-capability gate (maki/fusion-plan.f FP-BACKEND-EMITS?, dot cad-matmul-prologue)
\ now refuses EW->MATMUL, so GELU stays its own region 0 and never enters the matmul region
\ (region 1) via checked planning. Forcing GELU (node 0) into the matmul region simulates the
\ bad prologue-fused plan LMM-ANALYZE must still reject - the guard is now unreachable by
\ construction, so this hand-built plan proves the defense-in-depth guard directly.
MODEL: GM ( x:8x8 w:8x8 -- y ) GELU MATMUL ;
FP-BUILD
1 0 cells FP-RID + !                       \ poke GELU (node 0) into the matmul region (region 1)
' LMMT-TRY1 E-LMM-PROLOGUE TTHROWS

\ ---- fail closed: the contraction inner dim K exceeds the v1 accumulation cap ------
MODEL: WK ( x:8x512 w:512x8 -- y ) MATMUL ;
FP-BUILD
' LMMT-TRY E-LMM-KCAP TTHROWS

\ ---- fail closed: an output/operand bigger than the launch arena (v1 4096 f32) -----
MODEL: WD ( x:128x64 w:64x128 -- y ) MATMUL ;
FP-BUILD
' LMMT-TRY E-LMM-DIMS TTHROWS

\ ---- fail closed: a corrupted plan with two materialized outputs in one region -----
\ The planner NEVER produces this - maki/fusion-mout-test.f proves exactly one materialized
\ output per region - so E-LMM-MULTIOUT is a defense-in-depth invariant guard, not a v1 cap.
\ FP-BUILD leaves MATMUL interior (mat=0) and RELU the sole output (mat=1). Forcing the
\ MATMUL node materialized simulates a bad plan; LMM-ANALYZE must not trust it.
MODEL: MO ( x:8x8 w:8x8 -- y ) MATMUL RELU ;
FP-BUILD
0 0= 0 MIR-NODE-ID MIR-MAT!
' LMMT-TRY E-LMM-MULTIOUT TTHROWS

\ ---- fail closed: a corrupted plan with two contractions in one region -------------
\ The planner splits a second contraction into its own region (SR-MATMUL boundary), so a
\ two-contraction region is unreachable in practice. Forcing the second MATMUL's region id
\ back to region 0 (FP-RID, defense-in-depth) simulates a bad plan the analyzer must reject.
MODEL: M2 ( x:8x8 w:8x8 v:8x8 -- y ) MATMUL RELU MATMUL ;
FP-BUILD
0 2 cells FP-RID + !
' LMMT-TRY E-LMM-MULTIMM TTHROWS

T-REPORT

;package
