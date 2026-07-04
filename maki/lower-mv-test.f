\ maki/lower-mv-test.f - host-side checked tests for the movement lowering (slice 4).
\
\ Off-device: builds captured models, lowers a movement region to PTX text (in-process
\ capture, src/arch/ptx/emit.f PTX-CAPTURE-ON), and asserts the two device legs of CAD-PLAN
\ 6.3. (A) DISSOLVED free movement folds into a compute kernel's base-pointer offset
\ (maki/move-view.f + lower-ew/red/mm): a slice bakes `add.u64 %rdN, %rdN, r0*cols*4` before the
\ unchanged op body; a reshape (identity flat) bakes none. (B) a MATERIALIZED movement region
\ lowers to a copy kernel (maki/lower-move.f): transpose = div/rem row remap, slice = offset copy,
\ concat = two-source branch, gather = f32-index round (cvt.rzi after +0.5) + indexed rows.
\ Then the fail-closed paths: a STAGED (transpose) fold into any compute region, an
\ un-materialized movement output, a multi-node copy region, and a non-slot movement operand.
\ No device, no ptxas - PTX validity + numerics are proven on the Orin by
\ maki/lower-mv-device-test.f. Load via maki/test.f.

require lib/test.f
require lib/string.f
require maki/cad.f
require maki/lower-ew.f
require maki/lower-red.f
require maki/lower-mm.f
require maki/lower-move.f

package MAKI

\ ---- captured-text assertions (LMVT- prefix: shares the dictionary with siblings) --
variable LMVT-VA  variable LMVT-VU
: LMVT-SAVE ( ptr u8 n -- )  LMVT-VU ! LMVT-VA ! ;
: LMVT$ ( -- ptr u8 n )  LMVT-VA @ LMVT-VU @ ;
: LMVT-IN     ( ptr u8 n -- )  LMVT$ 2swap CONTAINS? TTRUE ;         \ needle present
: LMVT-ABSENT ( ptr u8 n -- )  LMVT$ 2swap FIND-SUB 0 < TTRUE ;      \ needle absent

\ exactly-one: the needle occurs once (find it, then prove no second occurrence)
: LMVT-ONCE? ( ptr u8 n ptr u8 n -- bool ) {: ha:ptr hu:n na:ptr nu:n :}
   ha hu na nu FIND-SUB {: i1:n :}
   i1 0 < if false exit then
   ha i1 + nu +  hu i1 - nu -  na nu FIND-SUB 0 < ;
: LMVT-ONCE ( ptr u8 n -- )  LMVT$ 2swap LMVT-ONCE? TTRUE ;

: LMVT-CAP-MV  ( -- )  PTX-CAPTURE-ON  0 LMV-EMIT  PTX-CAPTURE-OFF  PTX-CAPTURE$ LMVT-SAVE ;
: LMVT-CAP-EW  ( -- )  PTX-CAPTURE-ON  0 LEW-EMIT  PTX-CAPTURE-OFF  PTX-CAPTURE$ LMVT-SAVE ;
: LMVT-CAP-RED ( -- )  PTX-CAPTURE-ON  0 LRED-EMIT PTX-CAPTURE-OFF  PTX-CAPTURE$ LMVT-SAVE ;
: LMVT-CAP-MM  ( -- )  PTX-CAPTURE-ON  0 LMM-EMIT  PTX-CAPTURE-OFF  PTX-CAPTURE$ LMVT-SAVE ;

\ ---- fail-closed probes (each acts on the current model / plan) --------------
: LMVT-TRY-MV  ( -- )  0 LMV-ANALYZE ;
: LMVT-TRY-MV1 ( -- )  1 LMV-ANALYZE ;
: LMVT-TRY-EW  ( -- )  0 LEW-ANALYZE ;
: LMVT-TRY-RED ( -- )  0 LRED-ANALYZE ;
: LMVT-TRY-MM  ( -- )  0 LMM-ANALYZE ;
: LMVT-TRY-MM1 ( -- )  1 LMM-ANALYZE ;

T-RESET

\ ================= (B) materialized movement copy kernels =====================

\ ---- TRANSPOSE 4x8 as the model output: FP-MAT-FLAG now materializes it (dot maki-fusion-plan),
\ so the copy kernel runs without forcing - div/rem row remap, one buffer, no shared mem.
MODEL: TP ( x:4x8 -- y ) TRANSPOSE ;
FP-BUILD
0 MIR-MAT@ TTRUE                             \ mat-flag fix: the trailing movement output materializes
LMVT-CAP-MV
s" .visible .entry REGION_0"    LMVT-ONCE
s" .param .u64 p_in0"           LMVT-IN
s" .param .u64 p_out"           LMVT-IN
s" .param .u32 p_a"             LMVT-IN
s" .param .u32 p_n"             LMVT-IN
s" div.u32 %r8, %r7, %r1"       LMVT-IN      \ i = e / src_rows
s" rem.u32 %r9, %r7, %r1"       LMVT-IN      \ j = e mod src_rows
s" mad.lo.u32 %r10, %r9, %r2"   LMVT-IN      \ src = j*src_cols + i
s" ld.global.f32 %f1"           LMVT-IN
s" st.global.f32"               LMVT-IN
s" .param .u64 p_in1"           LMVT-ABSENT  \ transpose reads one buffer
s" .shared"                     LMVT-ABSENT  \ pure copy: no shared staging

\ ---- SLICE 1..3 of 4x2: r0*cols = 2 is not lane-aligned (verdict materialize -> mat=1),
\ so this standalone slice is its own copy region: offset copy, no div/rem.
MODEL: SL ( x:4x2 -- y ) SLICE:1..3 ;
FP-BUILD
LMVT-CAP-MV
s" .visible .entry REGION_0"    LMVT-ONCE
s" add.u32 %r8, %r7, %r1"       LMVT-IN      \ src flat = e + p_a (r0*cols)
s" div.u32"                     LMVT-ABSENT  \ offset copy, not a row remap
s" .param .u64 p_in1"           LMVT-ABSENT

\ ---- CONCAT 4x8 + 4x8: two-source branch copy.
MODEL: CT ( a:4x8 b:4x8 -- y ) CONCAT ;
FP-BUILD
LMVT-CAP-MV
s" .param .u64 p_in0"           LMVT-IN
s" .param .u64 p_in1"           LMVT-IN      \ two source buffers
s" setp.lt.u32 %p2, %r7, %r1"   LMVT-IN      \ e < split ?
s" FROMA:"                      LMVT-IN
s" AFTER:"                      LMVT-IN
s" cvt.rzi"                     LMVT-ABSENT  \ no index rounding here

\ ---- GATHER 8x8 by 4x1: f32 index rounded EXACTLY like EX-BUILD-IDX, then indexed rows.
MODEL: GA ( x:8x8 i:4x1 -- y ) GATHER ;
FP-BUILD
LMVT-CAP-MV
s" .param .u64 p_in1"           LMVT-IN      \ the f32 index buffer
s" add.f32 %f3, %f2, 0f3F000000" LMVT-IN     \ + 0.5 (mirror EX-BUILD-IDX)
s" cvt.rzi.s32.f32 %r10, %f3"   LMVT-IN      \ truncate toward zero
s" mad.lo.u32 %r11, %r10, %r1"  LMVT-IN      \ src = idx*C + c

\ ================= (A) dissolved free movement folded into index math =========

\ ---- SLICE 1..3 GELU of 4x8: free (offset 8 elems = 32 bytes) folds into the EW load base.
MODEL: SG ( x:4x8 -- y ) SLICE:1..3 GELU ;
FP-BUILD
LMVT-CAP-EW
s" .visible .entry REGION_0"    LMVT-ONCE
s" add.u64 %rd1, %rd1, 32"      LMVT-IN      \ slice base offset r0*cols*4
s" ex2.approx.f32"              LMVT-IN      \ the gelu body is unchanged
s" .param .u64 p_in1"           LMVT-ABSENT  \ one region input (the slice source)

\ ---- SLICE 0..2 GELU of 4x8: r0 = 0, so the fold bakes NO offset (identity base).
MODEL: SG0 ( x:4x8 -- y ) SLICE:0..2 GELU ;
FP-BUILD
LMVT-CAP-EW
s" add.u64 %rd1, %rd1,"         LMVT-ABSENT  \ zero offset: no base add emitted
s" ex2.approx.f32"              LMVT-IN

\ ---- RESHAPE 8x4 RELU of 4x8: contiguous reshape is identity flat -> no offset.
MODEL: RR ( x:4x8 -- y ) RESHAPE:8x4 RELU ;
FP-BUILD
LMVT-CAP-EW
s" add.u64 %rd1, %rd1,"         LMVT-ABSENT
s" max.f32"                     LMVT-IN      \ relu body unchanged

\ ---- SLICE 1..3 RMSNORM of 4x8: free offset folds into the RED row-span base.
MODEL: SR ( x:4x8 -- y ) SLICE:1..3 RMSNORM ;
FP-BUILD
LMVT-CAP-RED
s" add.u64 %rd1, %rd1, 32"      LMVT-IN
s" sqrt.rn.f32"                 LMVT-IN      \ rmsnorm body unchanged

\ ---- SLICE 2..6 MATMUL of 8x8 @ 8x16: free A offset (r0*K*4 = 64) folds into the K-loop base.
MODEL: SM ( x:8x8 w:8x16 -- y ) SLICE:2..6 MATMUL ;
FP-BUILD
LMVT-CAP-MM
s" add.u64 %rd1, %rd1, 64"      LMVT-IN
s" fma.rn.f32"                  LMVT-IN      \ contraction K-loop unchanged

\ ================= fail-closed boundaries ====================================

\ ---- STAGED (transpose) fold is unsupported v1 in EW / RED / MM (a lane permutation) -------
MODEL: TG ( x:4x8 -- y ) TRANSPOSE GELU ;
FP-BUILD
' LMVT-TRY-EW E-MVW-STAGED TTHROWS

MODEL: TN ( x:4x8 -- y ) TRANSPOSE RMSNORM ;
FP-BUILD
' LMVT-TRY-RED E-MVW-STAGED TTHROWS

MODEL: TM ( x:8x8 w:8x16 -- y ) TRANSPOSE MATMUL ;
FP-BUILD
' LMVT-TRY-MM E-MVW-STAGED TTHROWS

\ ---- a non-movement EW op feeding the contraction is a rejected prologue (hand-forced) -----
\ The backend-capability gate (maki/fusion-plan.f, dot cad-matmul-prologue) now refuses
\ EW->MATMUL, so GELU stays its own region 0 and never fuses into the matmul region (region 1).
\ Forcing GELU (node 0) into the matmul region simulates the bad prologue-fused plan the matmul
\ analyzer must still reject (defense-in-depth, now unreachable by checked planning).
MODEL: GM ( x:8x8 w:8x8 -- y ) GELU MATMUL ;
FP-BUILD
1 0 cells FP-RID + !                          \ poke GELU (node 0) into the matmul region (region 1)
' LMVT-TRY-MM1 E-LMM-PROLOGUE TTHROWS

\ ---- an un-materialized movement copy region has no output (hand-forced, defense-in-depth) --
\ FP-MAT-FLAG now materializes a trailing movement model-output (dot maki-fusion-plan), so a
\ standalone TRANSPOSE lowers without forcing. Forcing its mat flag back to 0 simulates the old
\ un-materialized plan the copy analyzer must still reject (E-LMV-NOOUT is now unreachable by
\ checked planning, so this hand-built plan proves the guard directly).
MODEL: TU ( x:4x8 -- y ) TRANSPOSE ;
FP-BUILD  0 0 MIR-MAT!
' LMVT-TRY-MV E-LMV-NOOUT TTHROWS

\ ---- a copy region must be exactly one movement node -------------------------------------
\ GELU then TRANSPOSE fuse (staged) into one region; the transpose is the model output, so
\ FP-MAT-FLAG materializes it (mat-flag fix), but LMV-ANALYZE rejects the two-node region as
\ multi-node (E-LMV-MULTI) before any output check - a copy region is exactly one movement node.
MODEL: GT ( x:4x8 -- y ) GELU TRANSPOSE ;
FP-BUILD
' LMVT-TRY-MV E-LMV-MULTI TTHROWS

\ ---- cross-region (slice 5): a materialized movement reading a producer NODE now lowers -------
\ GELU then CONCAT: gelu is region 0, concat materializes as region 1; concat's A operand is the
\ interior gelu node (materialized, so it has a device buffer). LMV-ANALYZE succeeds and stages
\ operand 0 as the gelu node (a cross-region producer) and operand 1 as the declared b input slot;
\ the whole-model run binds gelu's device buffer for the copy kernel (dot habu-maki-cross-region).
MODEL: GC ( x:4x8 b:4x8 -- y ) GELU CONCAT ;
FP-BUILD
LMVT-TRY-MV1                                  \ 1 LMV-ANALYZE: no longer throws
0 LMV-IN-REF@ MIR-REF-INPUT? TFALSE           \ operand 0 is a node (the gelu producer), not a slot
0 LMV-IN-REF@ 0 T=                            \ specifically node 0 (gelu)
1 LMV-IN-REF@ MIR-REF-INPUT? TTRUE            \ operand 1 is the declared b input slot

\ ---- an UN-materialized node operand still fails closed (no device buffer to bind) -----------
\ Force the gelu producer mat=0 (a corrupted plan): the concat's A operand is now an interior
\ node with no device buffer, so the copy region rejects it (defense-in-depth, E-LMV-INPUT).
MODEL: GC2 ( x:4x8 b:4x8 -- y ) GELU CONCAT ;
FP-BUILD  0 0 MIR-MAT!
' LMVT-TRY-MV1 E-LMV-INPUT TTHROWS

T-REPORT

end-package
