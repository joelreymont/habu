\ maki/bcast-mul-op-test.f - the 1xC broadcast multiply op through the model-op path
\ (dot habu-add-1xc-broadcast). OP-BCAST-MUL is the multiplicative twin of OP-BIAS:
\ AxC data times a 1xC row vector, broadcast over rows (a general per-channel scale -
\ attention scaling, SwiGLU-style gating). This proves the op integration end to end:
\
\   (1) HOST GOLDEN: MODEL: capture of the binary op, the host executor's 1xC broadcast
\       read (EX-BC@) gives y[r,c] = x[r,c]*g[c] exactly (integer-valued fixture).
\   (2) SHAPE GUARD: EW-SHAPE-CHECK under the BCAST-MUL class rejects a non-1xC param
\       (E-CAD-PARAM-SHAPE, the cad-test TRY-SCALE-BAD idiom); a legal 1xC passes.
\   (3) GRADCHECK: the captured backward is FD-verified END TO END by the host gradcheck
\       (GC-RUN builds the backward IR and central-differences the seeded loss w.r.t.
\       BOTH inputs - dx = ct*g and d-g = rowsum(ct.*x)), the adam-attn-grad part-C idiom.
\   (4) FUSION: bcast-mul is CLASS-EW, so it fuses into a reduction epilogue like BIAS -
\       RMSNORM then BCAST-MUL plans exactly ONE region, and the fused composition still
\       gradchecks (stays golden through the reduction+epilogue path).
\
\ The device leg (the flat kernel's 1xC mul.rn.f32 with the EX-BC@-matching mod-C offset)
\ is a lower-ew PTX golden in maki/lower/ew-test.f; the on-GPU numeric golden is
\ maki/lower/device-test.f (LD-GOLD1 -> V-PASS).

require lib/test.f
require lib/float.f
require lib/string.f
require maki/array.f
require maki/cad.f
require maki/gradcheck.f
require maki/fusion-plan.f

package MAKI

: BMO>I ( ptr a n -- n )  T-GET 0.5 f+ f>s ;   \ read an output cell as nearest int

\ ---- shape-guard fixtures (EW-SHAPE-CHECK with the BCAST-MUL broadcast class) ----
\ A 1xC param row-broadcasts over AxC data (legal); a 2xC param is not a row broadcast.
: BMO-BAD ( -- )                              \ g 2x3 vs x 2x3: not 1xC -> reject
   TENSOR:TV-RESET
   2 3 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC
   2 3 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC
   MAKI-OPKIND:BCAST-MUL EW-SHAPE-CHECK ;
: BMO-OK ( -- )                               \ g 1x3 vs x 2x3: legal row broadcast
   TENSOR:TV-RESET
   2 3 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC
   1 3 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC
   MAKI-OPKIND:BCAST-MUL EW-SHAPE-CHECK ;
: BMO-OK-THROWS ( -- n )  [: BMO-OK ;] catch ;   \ positive control: a legal 1xC must NOT throw

\ ---- host golden fixture: x is 2x3, g is 1x3, y[r,c] = x[r,c]*g[c] --------------
3 constant BMO-C   6 constant BMO-RC
create BMO-X BMO-RC cells allot   create BMO-G BMO-C cells allot
: BMO-FILL ( -- )
   1.0 BMO-X 0 T-SET  2.0 BMO-X 1 T-SET  3.0 BMO-X 2 T-SET
   4.0 BMO-X 3 T-SET  5.0 BMO-X 4 T-SET  6.0 BMO-X 5 T-SET
   10.0 BMO-G 0 T-SET  100.0 BMO-G 1 T-SET  1000.0 BMO-G 2 T-SET ;
: BMO-BIND-RUN ( -- )                         \ bind the two inputs and run the forward IR
   BMO-FILL EX-RESET
   BMO-X 0 MIR-SLOT-ID EX-BIND  BMO-G 1 MIR-SLOT-ID EX-BIND  EX-RUN ;
: BMO-OUT ( -- ptr a )  0 MIR-NODE-ID EX-OUT@ ;

T-RESET

\ ---- (1) host golden: AxC * 1xC broadcasts per column --------------------------
MODEL: BMO-M ( x:2x3 g:1x3 -- y ) BCAST-MUL ;
MODEL-K 1 T=                                             \ one op node
0 MIR-NODE-ID MIR-OP@ OPR-NAME s" bcast-mul" STR= TTRUE  \ the new OP-BCAST-MUL
0 MIR-NODE-ID MIR-IN-COUNT@ 2 T=                         \ arity 2 (data + 1xC param)
BMO-BIND-RUN
BMO-OUT 0 BMO>I 10   T=                                  \ 1*10
BMO-OUT 1 BMO>I 200  T=                                  \ 2*100
BMO-OUT 2 BMO>I 3000 T=                                  \ 3*1000
BMO-OUT 3 BMO>I 40   T=                                  \ 4*10   (row 1 reuses the same 1xC)
BMO-OUT 4 BMO>I 500  T=                                  \ 5*100
BMO-OUT 5 BMO>I 6000 T=                                  \ 6*1000

\ ---- (2) wrong-shape rejects (2xC), a legal 1xC passes -------------------------
' BMO-BAD E-CAD-PARAM-SHAPE TTHROWS
BMO-OK-THROWS 0 T=

\ ---- (3) gradcheck: dx AND d-g match central FD -------------------------------
MODEL: BMO-GC ( x:2x3 g:1x3 -- y ) BCAST-MUL ;
GC-RUN V-PASS T=                                         \ both inputs FD-verified
GC-RE$ s" 2 input(s) gradchecked" CONTAINS? TTRUE

\ ---- (4) fuses into a reduction epilogue: one region, stays golden ------------
MODEL: BMO-FUSE ( x:4x8 g:1x8 -- y ) RMSNORM BCAST-MUL ;
FP-BUILD
FP-REGION-COUNT 1 T=                                     \ EW epilogue folds into the reduction region
MODEL: BMO-FUSE-GC ( x:4x8 g:1x8 -- y ) RMSNORM BCAST-MUL ;
GC-RUN V-PASS T=                                         \ the reduction+epilogue composition stays golden

T-REPORT

;package
