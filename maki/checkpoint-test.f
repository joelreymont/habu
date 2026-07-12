\ maki/checkpoint-test.f - segment-boundary checkpointing vs the full-
\ materialization executor path (docs/maki/train.md "Gradient checkpointing" -
\ the spec this asserts). Proves, deterministically: BIT-IDENTICAL (exact f=)
\ losses, every input gradient, and the rematerialized interior on the
\ committed MLP + attention models AND a 3-linear MLP whose interiors span two
\ segments (there the scratch overlay really destroys n0 during CK-FWD and
\ CK-BWD recomputes it - remats=1); the committed live-buffer accounting
\ (saved-across-the-cut strictly lower on every multi-segment model, peak live
\ strictly lower on the two-interior-segment model); and the fail-closed paths
\ (accessors/runs before CK-SETUP, non-interval segments, a backward node
\ reading interiors of two segments).

require lib/test.f
require lib/float.f
\ flatten the require chain for standalone runs (INCLUDE-MAX-DEPTH): leaves first
require maki/op-kind.f
require maki/op-registry.f
require maki/move-facts.f
require maki/tensor.f
require maki/tensor-value.f
require maki/model-ir.f
require maki/array.f
require maki/autograd.f
require maki/adjoint.f
require maki/fusion-plan.f
require maki/backward.f
require maki/executor.f
require maki/report.f
require maki/plan-vocab.f
require maki/cad.f
require maki/from-scratch-model.f
require maki/loss.f
require maki/from-scratch-train.f
require maki/optim-tensor.f
require maki/loss-tensor.f
require maki/adam-train.f
require maki/checkpoint.f

package MAKI

\ ---- shared helpers ----------------------------------------------------------
: CKT-COPY ( ptr a ptr a n -- ) {: src:ptr dst:ptr n:n :}
   n 0 ?do  src i T-GET  dst i T-SET  loop ;

: CKT-EQ? ( ptr a ptr a n -- bool ) {: a:ptr b:ptr n:n :}   \ exact f= per element
   n 0 ?do  a i T-GET b i T-GET f= 0= if false unloop exit then  loop  true ;

\ ---- fail closed: run / accessor before any CK-SETUP -------------------------
: CKT-THROW-SEG ( -- )  CK-SEG-N@ drop ;

\ ---- MLP: baseline grads under EX-RUN, then the checkpointed re-run ----------
create MGX  SC-XN  cells allot
create MGW1 SC-W1N cells allot
create MGB1 SC-B1N cells allot
create MGW2 SC-W2N cells allot
create MGB2 SC-B2N cells allot
create MN0  SC-BATCH SC-HID * cells allot     \ interior node 0 (8x16)
variable CKM-L

: CKT-MLP-BASE ( -- )                \ precondition: fresh SCRATCH-MLP capture
   SC-SETUP
   BW-FWD-N@ EX-RUN-N
   SC-LOSS-SEED CKM-L !
   EX-RUN
   SC-X-SLOT  SC-GRAD-AT MGX  SC-XN  CKT-COPY
   SC-W1-SLOT SC-GRAD-AT MGW1 SC-W1N CKT-COPY
   SC-B1-SLOT SC-GRAD-AT MGB1 SC-B1N CKT-COPY
   SC-W2-SLOT SC-GRAD-AT MGW2 SC-W2N CKT-COPY
   SC-B2-SLOT SC-GRAD-AT MGB2 SC-B2N CKT-COPY
   0 MIR-NODE-ID EX-OUT@ MN0 SC-BATCH SC-HID * CKT-COPY ;

: CKT-MLP-CKPT ( -- r )              \ precondition: fresh SCRATCH-MLP capture
   SC-INIT-PARAMS
   SC-GEN-DATA
   CK-SETUP
   EX-RESET
   SC-X  SC-X-SLOT  EX-BIND
   SC-W1 SC-W1-SLOT MIR-SLOT-ID EX-BIND
   SC-B1 SC-B1-SLOT MIR-SLOT-ID EX-BIND
   SC-W2 SC-W2-SLOT MIR-SLOT-ID EX-BIND
   SC-B2 SC-B2-SLOT MIR-SLOT-ID EX-BIND
   SC-SEED BW-SEED-SLOT@ EX-BIND
   CK-FWD
   SC-LOSS-SEED {: loss:r :}
   CK-BWD
   loss ;

\ ---- attention: same shape as the Adam trainer's committed model -------------
create AGQ ATN-EN cells allot
create AGK ATN-EN cells allot
create AGS ATN-SN cells allot
create AGV ATN-EN cells allot
create AN0 16 cells allot                     \ interior node 0 = q@kt (4x4)
variable CKA-L

: CKT-ATN-BASE ( -- )                \ precondition: fresh ADAM-ATTN capture
   ATN-SETUP
   ATN-GRADS CKA-L !
   ATN-Q-SLOT  SC-GRAD-AT AGQ ATN-EN CKT-COPY
   ATN-KT-SLOT SC-GRAD-AT AGK ATN-EN CKT-COPY
   ATN-S-SLOT  SC-GRAD-AT AGS ATN-SN CKT-COPY
   ATN-V-SLOT  SC-GRAD-AT AGV ATN-EN CKT-COPY
   0 MIR-NODE-ID EX-OUT@ AN0 16 CKT-COPY ;

: CKT-ATN-CKPT ( -- r )              \ precondition: fresh ADAM-ATTN capture
   ATN-INIT
   CK-SETUP
   EX-RESET
   ATN-Q   ATN-Q-SLOT  EX-BIND
   ATN-KT  ATN-KT-SLOT MIR-SLOT-ID EX-BIND
   ATN-S   ATN-S-SLOT  EX-BIND
   ATN-V   ATN-V-SLOT  EX-BIND
   ATN-SEED BW-SEED-SLOT@ EX-BIND
   CK-FWD
   ATN-LOSS-SEED {: loss:r :}
   CK-BWD
   loss ;

\ ---- 3-linear MLP: interiors in TWO segments (the overlay really recomputes) --
\ x:8x6 -LINEAR-> 8x16 -GELU-> -LINEAR-> 8x16 -GELU-> -LINEAR-> 8x2.
\ Segments {n0,n1} {n2,n3} {n4}; interiors n0 (seg0) and n2 (seg1) OVERLAY the
\ same scratch cells, so n0 is destroyed by CK-FWD and recomputed by CK-BWD.
48  constant DP-XN
96  constant DP-W1N
16  constant DP-B1N
256 constant DP-W2N
16  constant DP-B2N
32  constant DP-W3N
2   constant DP-B3N
16  constant DP-SEEDN                \ 8x2 output cotangent
128 constant DP-N0N                  \ 8x16 interior

create DP-X  DP-XN  cells allot   create DP-W1 DP-W1N cells allot
create DP-B1 DP-B1N cells allot   create DP-W2 DP-W2N cells allot
create DP-B2 DP-B2N cells allot   create DP-W3 DP-W3N cells allot
create DP-B3 DP-B3N cells allot   create DP-SEED DP-SEEDN cells allot

create DGX  DP-XN  cells allot    create DGW1 DP-W1N cells allot
create DGB1 DP-B1N cells allot    create DGW2 DP-W2N cells allot
create DGB2 DP-B2N cells allot    create DGW3 DP-W3N cells allot
create DGB3 DP-B3N cells allot    create DN0  DP-N0N cells allot

\ deterministic varied fill, distinct stream per buffer (k)
: DP-FILL ( ptr a n n -- ) {: p:ptr n:n k:n :}
   n 0 ?do  k i 3 * + 17 mod s>f 0.07 f* 0.15 f+  p i T-SET  loop ;

: DP-FILLS ( -- )                    \ params/input + a non-uniform seed cotangent
   DP-X  DP-XN  1 DP-FILL   DP-W1 DP-W1N 2 DP-FILL   DP-B1 DP-B1N 3 DP-FILL
   DP-W2 DP-W2N 4 DP-FILL   DP-B2 DP-B2N 5 DP-FILL
   DP-W3 DP-W3N 6 DP-FILL   DP-B3 DP-B3N 7 DP-FILL
   DP-SEEDN 0 ?do  i 5 mod s>f 0.11 f* 0.4 f+  DP-SEED i T-SET  loop ;

: DP-BIND ( -- )                     \ slots 0..6 in signature order + the seed
   EX-RESET
   DP-X  0 MIR-SLOT-ID EX-BIND  DP-W1 1 MIR-SLOT-ID EX-BIND  DP-B1 2 MIR-SLOT-ID EX-BIND
   DP-W2 3 MIR-SLOT-ID EX-BIND  DP-B2 4 MIR-SLOT-ID EX-BIND  DP-W3 5 MIR-SLOT-ID EX-BIND  DP-B3 6 MIR-SLOT-ID EX-BIND
   DP-SEED BW-SEED-SLOT@ EX-BIND ;

: CKT-DEEP-BASE ( -- )               \ precondition: fresh CKPT-DEEP capture
   DP-FILLS
   BW-BUILD
   DP-BIND
   EX-RUN
   0 SC-GRAD-AT DGX  DP-XN  CKT-COPY
   1 SC-GRAD-AT DGW1 DP-W1N CKT-COPY
   2 SC-GRAD-AT DGB1 DP-B1N CKT-COPY
   3 SC-GRAD-AT DGW2 DP-W2N CKT-COPY
   4 SC-GRAD-AT DGB2 DP-B2N CKT-COPY
   5 SC-GRAD-AT DGW3 DP-W3N CKT-COPY
   6 SC-GRAD-AT DGB3 DP-B3N CKT-COPY
   0 MIR-NODE-ID EX-OUT@ DN0 DP-N0N CKT-COPY ;

: CKT-DEEP-CKPT ( -- )               \ precondition: fresh CKPT-DEEP capture
   DP-FILLS
   CK-SETUP
   DP-BIND
   CK-FWD
   CK-BWD ;

\ ---- fail closed: non-interval segments ---------------------------------------
\ n0 gelu(i0) opens seg0; n1 matmul(i0,i1) opens seg1; n2 relu(n0) rejoins seg0
\ -> seg0 = {0,2} is not an interval -> CK-SETUP throws before BW-BUILD.
: CKT-IN+ ( -- )  2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop ;
: CKT-U+ ( MIR:operand-ref opkind -- )   \ unary 2x2 node over operand ref
   MIR-OP-BEGIN MIR-IN+ 2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop ;
: CKT-MM+ ( MIR:operand-ref MIR:operand-ref -- )   \ matmul 2x2 node over two operand refs
   MAKI-OPKIND:MATMUL MIR-OP-BEGIN {: a:MIR:operand-ref b:MIR:operand-ref :}
   a MIR-IN+  b MIR-IN+
   2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop ;

: CKT-TRY-INTERVAL ( -- )
   MIR-RESET  CKT-IN+  CKT-IN+
   0 MIR-SLOT-ID MIR-IN-REF MAKI-OPKIND:GELU CKT-U+
   0 MIR-SLOT-ID MIR-IN-REF 1 MIR-SLOT-ID MIR-IN-REF CKT-MM+
   0 MIR-NODE-ID MIR-NODE-REF MAKI-OPKIND:RELU CKT-U+
   CK-SETUP ;

\ ---- fail closed: one backward node reading interiors of two segments --------
\ forward: n0 gelu(i0), n1 relu(n0) [seg0; n0 interior], n2 matmul(n1,i1),
\ n3 silu(n2) [seg1; n2 interior]. After CK-SETUP a hand-appended MUL(n0,n2)
\ lands in the backward index range and reads BOTH interiors -> E-CK-CROSS.
create CXA 4 cells allot   create CXB 4 cells allot   create CXS 4 cells allot

: CKT-CROSS-FILL ( -- )
   4 0 ?do  i 1+ s>f 0.25 f*  CXA i T-SET  loop
   4 0 ?do  i 2 + s>f 0.5 f*  CXB i T-SET  loop
   4 0 ?do  i 1+ s>f 0.1 f*   CXS i T-SET  loop ;

: CKT-TRY-CROSS ( -- )
   MIR-RESET  CKT-IN+  CKT-IN+
   0 MIR-SLOT-ID MIR-IN-REF MAKI-OPKIND:GELU CKT-U+
   0 MIR-NODE-ID MIR-NODE-REF MAKI-OPKIND:RELU CKT-U+
   1 MIR-NODE-ID MIR-NODE-REF 1 MIR-SLOT-ID MIR-IN-REF CKT-MM+
   2 MIR-NODE-ID MIR-NODE-REF MAKI-OPKIND:SILU CKT-U+
   CK-SETUP
   CKT-CROSS-FILL
   EX-RESET  CXA 0 MIR-SLOT-ID EX-BIND  CXB 1 MIR-SLOT-ID EX-BIND  CXS BW-SEED-SLOT@ EX-BIND
   CK-FWD
   MAKI-OPKIND:MUL MIR-OP-BEGIN 0 MIR-NODE-ID MIR-NODE-REF MIR-IN+ 2 MIR-NODE-ID MIR-NODE-REF MIR-IN+
   2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
   CK-BWD ;

T-RESET

\ ---- fail closed BEFORE any setup: every entry point guards on CK-BUILT? ------
' CKT-THROW-SEG E-CK-STATE TTHROWS
' CK-FWD        E-CK-STATE TTHROWS
' CK-BWD        E-CK-STATE TTHROWS

\ ---- MLP: bit-identical loss, every gradient, and the interior ---------------
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
CKT-MLP-BASE
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
CKT-MLP-CKPT CKM-L @ f= TTRUE
SC-X-SLOT  SC-GRAD-AT MGX  SC-XN  CKT-EQ? TTRUE
SC-W1-SLOT SC-GRAD-AT MGW1 SC-W1N CKT-EQ? TTRUE
SC-B1-SLOT SC-GRAD-AT MGB1 SC-B1N CKT-EQ? TTRUE
SC-W2-SLOT SC-GRAD-AT MGW2 SC-W2N CKT-EQ? TTRUE
SC-B2-SLOT SC-GRAD-AT MGB2 SC-B2N CKT-EQ? TTRUE
0 MIR-NODE-ID EX-OUT@ MN0 SC-BATCH SC-HID * CKT-EQ? TTRUE

\ committed plan facts: 2 segments {linear,gelu}{linear}, n0 the one interior;
\ one interior segment -> the scratch still holds it after CK-FWD -> ZERO remats
CK-SEG-N@ 2 T=
CK-BND-N@ 2 T=
CK-IMAX@  1 T=
CK-REMATS@ 0 T=
CK-SAVED@ 2 T=
CK-SAVED-FULL@ 3 T=
CK-SAVED@ CK-SAVED-FULL@ < TTRUE               \ strictly fewer saved buffers
CK-LIVE@ CK-LIVE-FULL@ <= TTRUE                \ peak equal here (one interior total)

\ ---- attention: bit-identical loss, every gradient, and the interior ----------
MODEL: ADAM-ATTN ( q:4x3 kt:3x4 s:1x1 v:4x3 -- o ) MATMUL SCALE SOFTMAX-ROW MATMUL ;
CKT-ATN-BASE
MODEL: ADAM-ATTN ( q:4x3 kt:3x4 s:1x1 v:4x3 -- o ) MATMUL SCALE SOFTMAX-ROW MATMUL ;
CKT-ATN-CKPT CKA-L @ f= TTRUE
ATN-Q-SLOT  SC-GRAD-AT AGQ ATN-EN CKT-EQ? TTRUE
ATN-KT-SLOT SC-GRAD-AT AGK ATN-EN CKT-EQ? TTRUE
ATN-S-SLOT  SC-GRAD-AT AGS ATN-SN CKT-EQ? TTRUE
ATN-V-SLOT  SC-GRAD-AT AGV ATN-EN CKT-EQ? TTRUE
0 MIR-NODE-ID EX-OUT@ AN0 16 CKT-EQ? TTRUE

\ committed plan facts: 3 segments {matmul,scale}{softmax}{matmul}; the interior
\ q@kt matmul sits in seg0 alone -> still zero remats (spec: recompute begins
\ when >= 2 segments carry interiors)
CK-SEG-N@ 3 T=
CK-BND-N@ 3 T=
CK-IMAX@  1 T=
CK-REMATS@ 0 T=
CK-SAVED@ 3 T=
CK-SAVED-FULL@ 4 T=
CK-SAVED@ CK-SAVED-FULL@ < TTRUE
CK-LIVE@ CK-LIVE-FULL@ <= TTRUE

\ ---- 3-linear MLP: the overlay destroys n0; CK-BWD recomputes it BIT-IDENTICAL -
MODEL: CKPT-DEEP ( x:8x6 w1:6x16 b1:1x16 w2:16x16 b2:1x16 w3:16x2 b3:1x2 -- y ) LINEAR GELU LINEAR GELU LINEAR ;
CKT-DEEP-BASE
MODEL: CKPT-DEEP ( x:8x6 w1:6x16 b1:1x16 w2:16x16 b2:1x16 w3:16x2 b3:1x2 -- y ) LINEAR GELU LINEAR GELU LINEAR ;
CKT-DEEP-CKPT
0 SC-GRAD-AT DGX  DP-XN  CKT-EQ? TTRUE
1 SC-GRAD-AT DGW1 DP-W1N CKT-EQ? TTRUE
2 SC-GRAD-AT DGB1 DP-B1N CKT-EQ? TTRUE
3 SC-GRAD-AT DGW2 DP-W2N CKT-EQ? TTRUE
4 SC-GRAD-AT DGB2 DP-B2N CKT-EQ? TTRUE
5 SC-GRAD-AT DGW3 DP-W3N CKT-EQ? TTRUE
6 SC-GRAD-AT DGB3 DP-B3N CKT-EQ? TTRUE
0 MIR-NODE-ID EX-OUT@ DN0 DP-N0N CKT-EQ? TTRUE             \ the recomputed interior, exact

\ committed plan facts: interiors n0 (seg0) + n2 (seg1) -> exactly one remat
\ (seg1 is still live after CK-FWD; seg0 is re-run when the gelu adjoint needs n0)
CK-SEG-N@ 3 T=
CK-BND-N@ 3 T=
CK-IMAX@  1 T=
CK-REMATS@ 1 T=
CK-SAVED@ 3 T=
CK-SAVED-FULL@ 5 T=
CK-SAVED@ CK-SAVED-FULL@ < TTRUE
CK-LIVE@ CK-LIVE-FULL@ < TTRUE                 \ peak STRICTLY lower (two interiors share)

\ ---- fail closed: v1 domain guards --------------------------------------------
' CKT-TRY-INTERVAL E-CK-INTERVAL TTHROWS
' CKT-TRY-CROSS    E-CK-CROSS    TTHROWS

T-REPORT

end-package
