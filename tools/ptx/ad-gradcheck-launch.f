\ ad-gradcheck-launch.f - device-run per-VJP-entry central-difference gradcheck.
\
\ The HARD GATE half that runs ON the Orin: for every VJP entry kernel pair
\ emitted by tools/ptx/ad-entry-lib.f (AD_FWD/AD_BWD cubins in a private scratch
\ dir given as the script argument), form the numeric gradient by central
\ differences over the SAME emitted forward and compare it per-element against
\ the analytic AD backward with rtol+atol. Positive fixtures per entry: fixed
\ irregular values ("randomized", reproducible), tie inputs where the function
\ is smooth (softmax; x/sum - x-max is kinked at ties so its tie case is
\ excluded by design), and saturated logits. Negative fixtures prove the gate
\ DISCRIMINATES on device: the wrong-VJP kernel (DUP fan-out cotangent dropped)
\ and a cross-paired backward must both mismatch. Every CUDA rc is checked
\ (CUDA:RC0 fail-closed; a missing cubin dies nonzero) and each readback is
\ poisoned with a NaN canary so a dropped copy-back fails closed.
\
\ Ships to the device box and runs from the checkout there (legacy CUDA:CU*
\ spellings + local f64-cell helpers keep it loadable on both trees):
\ ./bin/hb --load lib/errors.f lib/string.f lib/test.f lib/float.f lib/fmt.f
\   src/arch/ptx/emit.f lib/ptx/cg.f lib/ptx/header.f lib/ptx/launch.f
\   lib/ffi.f maki/cuda-types.f maki/cuda-driver.f
\   <scratch>/ad-gradcheck-launch.f -- <scratch>

4 constant AGK                     \ row width k
16 constant AGBYTES                \ k f32 cells
256 constant AGBLOCK
$AD constant AG-POISON-B           \ readback poison fill byte
$ADADADAD constant AG-POISON-W     \ poisoned f32 word (never a kernel output)

create AG-DIR 256 allot
create AG-PZ 256 allot             \ cstr scratch: cubin path
create AG-KN 16 allot              \ cstr scratch: kernel name
create AG-IN AGBYTES allot         \ f32 staging: x / dz upload
create AG-DYB AGBYTES allot        \ f32 staging: dy upload
create AG-OUT AGBYTES allot        \ f32 staging: readback
create AG-X 4 cells allot          \ host f64: input
create AG-Y 4 cells allot          \ host f64: second input (AD2/ADF kernels)
create AG-AH 1 cells allot         \ host f64: scalar factor a (ADS/ADF kernels)
create AG-DY 4 cells allot         \ host f64: output cotangent
create AG-YP 4 cells allot         \ host f64: forward at x+h
create AG-YM 4 cells allot         \ host f64: forward at x-h
create AG-DXN 4 cells allot        \ host f64: numeric gradient (input 1)
create AG-DXA 4 cells allot        \ host f64: analytic gradient (input 1)
create AG-DYN 4 cells allot        \ host f64: numeric gradient (input 2)
create AG-DYA 4 cells allot        \ host f64: analytic gradient (input 2)
create AG-DAN 1 cells allot        \ host f64: numeric scalar gradient da
create AG-DAA 1 cells allot        \ host f64: analytic scalar gradient da

variable AG-DIR-U
variable AG-DEV
variable AG-CTX
variable AG-MF
variable AG-MB
variable AG-FWD
variable AG-BWD
variable AG-dX
variable AG-dDY
variable AG-dO
variable AG-dY2
variable AG-dO2
variable AG-dO3
variable AG-AB                     \ f32 bits of the scalar a for cuParamSetv
variable AG-KV
variable AG-MISS#

\ ---- f64 host cells (local: maki/array.f spellings diverge across trees) ----

: AG-F@ ( ptr a n -- r )
   cells + @ ;

: AG-F! ( r ptr a n -- )
   cells + ! ;

\ ---- f32 staging pack/unpack -------------------------------------------------

: AG-F32! ( n ptr u8 n -- ) {: v:n buf:ptr idx:n :}
   idx 4 * {: o:n :}
   v $FF and buf o + c!  v 8 rshift $FF and buf o 1 + + c!
   v 16 rshift $FF and buf o 2 + + c!  v 24 rshift $FF and buf o 3 + + c! ;

: AG-F32@ ( ptr u8 n -- n ) {: buf:ptr idx:n :}
   idx 4 * {: o:n :}
   buf o + c@  buf o 1 + + c@ 8 lshift or
   buf o 2 + + c@ 16 lshift or  buf o 3 + + c@ 24 lshift or ;

: AG-PACK ( ptr a ptr u8 -- ) {: src:ptr dst:ptr :}
   AGK 0 ?do  src i AG-F@ F64>F32  dst i AG-F32!  loop ;

: AG-UNPACK ( ptr u8 ptr a -- ) {: src:ptr dst:ptr :}
   AGK 0 ?do  src i AG-F32@ F32>F64  dst i AG-F!  loop ;

\ ---- poisoned readback: a dropped copy-back fails closed ----------------------

: AG-POISON ( -- )
   AGBYTES 0 ?do
      AG-POISON-B AG-OUT i + c!
   loop ;

: AG-POISONED? ( n -- bool )
   AG-OUT swap AG-F32@ AG-POISON-W = ;

: AG-GUARD ( -- )
   0 AG-POISONED? 0= TTRUE ;

\ ---- CUDA plumbing (legacy spellings; every rc checked) -----------------------

: AG-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   SB-RESET
   AG-DIR AG-DIR-U @ SB-APPEND  s" /" SB-APPEND  a u SB-APPEND
   SB$ AG-PZ >CSTR ;

: AG-INIT ( -- )
   CUDA:OPEN
   0 CUDA:CUINIT CUDA:RC0
   AG-DEV 0 >IDX CUDA:CUDEVICEGET CUDA:RC0
   AG-CTX AG-DEV @ >CUDA-DEV CUDA:CUDEVICEPRIMARYCTXRETAIN CUDA:RC0
   AG-CTX @ >CUDA-CTX CUDA:CUCTXSETCURRENT CUDA:RC0
   AG-dX 16 >LEN CUDA:CUMEMALLOC CUDA:RC0
   AG-dDY 16 >LEN CUDA:CUMEMALLOC CUDA:RC0
   AG-dO 16 >LEN CUDA:CUMEMALLOC CUDA:RC0
   AG-dY2 16 >LEN CUDA:CUMEMALLOC CUDA:RC0
   AG-dO2 16 >LEN CUDA:CUMEMALLOC CUDA:RC0
   AG-dO3 16 >LEN CUDA:CUMEMALLOC CUDA:RC0
   AGK AG-KV ! ;

: AG-FINI ( -- )
   AG-dX @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   AG-dDY @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   AG-dO @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   AG-dY2 @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   AG-dO2 @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   AG-dO3 @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   AG-DEV @ >CUDA-DEV CUDA:CUDEVICEPRIMARYCTXRELEASE CUDA:RC0 ;

: AG-FWD-MOD! ( ptr u8 n ptr u8 n -- ) {: f:ptr fu:n kn:ptr knu:n :}
   f fu AG-PATH!
   AG-MF AG-PZ CUDA:CUMODULELOAD CUDA:RC0
   kn knu AG-KN >CSTR
   AG-FWD AG-MF @ >CUDA-MOD AG-KN CUDA:CUMODULEGETFUNCTION CUDA:RC0 ;

: AG-BWD-MOD! ( ptr u8 n ptr u8 n -- ) {: b:ptr bu:n kn:ptr knu:n :}
   b bu AG-PATH!
   AG-MB AG-PZ CUDA:CUMODULELOAD CUDA:RC0
   kn knu AG-KN >CSTR
   AG-BWD AG-MB @ >CUDA-MOD AG-KN CUDA:CUMODULEGETFUNCTION CUDA:RC0 ;

: AG-PAIR! ( ptr u8 n ptr u8 n -- ) {: f:ptr fu:n b:ptr bu:n :}
   f fu s" AD_FWD" AG-FWD-MOD!
   b bu s" AD_BWD" AG-BWD-MOD! ;

: AG2-PAIR! ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: f:ptr fu:n b:ptr bu:n fk:ptr fku:n bk:ptr bku:n :}
   f fu fk fku AG-FWD-MOD!
   b bu bk bku AG-BWD-MOD! ;

: AG-PAIR-CLOSE ( -- )
   AG-MF @ >CUDA-MOD CUDA:CUMODULEUNLOAD CUDA:RC0
   AG-MB @ >CUDA-MOD CUDA:CUMODULEUNLOAD CUDA:RC0 ;

\ ---- launches ------------------------------------------------------------------

: AG-FWD-RUN ( ptr a ptr a -- ) {: src:ptr dst:ptr :}
   AG-POISON
   1 AGK AGBLOCK PTX-ROW-LAUNCH-CHECK
   src AG-IN AG-PACK
   AG-dX @ >CUDA-DEVPTR AG-IN AGBYTES >LEN CUDA:CUMEMCPYHTOD CUDA:RC0
   AG-FWD @ >CUDA-FN AGBLOCK 1 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   AG-FWD @ >CUDA-FN 20 >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   AG-FWD @ >CUDA-FN 0 >IDX  AG-dX 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-FWD @ >CUDA-FN 8 >IDX  AG-dO 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-FWD @ >CUDA-FN 16 >IDX AG-KV 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-FWD @ >CUDA-FN 1 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   AG-OUT AG-dO @ >CUDA-DEVPTR AGBYTES >LEN CUDA:CUMEMCPYDTOH CUDA:RC0
   AG-GUARD
   AG-OUT dst AG-UNPACK ;

: AG-BWD-RUN ( -- )
   AG-POISON
   1 AGK AGBLOCK PTX-ROW-LAUNCH-CHECK
   AG-X AG-IN AG-PACK   AG-DY AG-DYB AG-PACK
   AG-dX @ >CUDA-DEVPTR AG-IN AGBYTES >LEN CUDA:CUMEMCPYHTOD CUDA:RC0
   AG-dDY @ >CUDA-DEVPTR AG-DYB AGBYTES >LEN CUDA:CUMEMCPYHTOD CUDA:RC0
   AG-BWD @ >CUDA-FN AGBLOCK 1 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   AG-BWD @ >CUDA-FN 28 >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   AG-BWD @ >CUDA-FN 0 >IDX  AG-dX 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 8 >IDX  AG-dDY 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 16 >IDX AG-dO 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 24 >IDX AG-KV 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 1 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   AG-OUT AG-dO @ >CUDA-DEVPTR AGBYTES >LEN CUDA:CUMEMCPYDTOH CUDA:RC0
   AG-GUARD
   AG-OUT AG-DXA AG-UNPACK ;

\ ---- central difference vs analytic --------------------------------------------

: AG-EPS ( -- r )
   1.0 4096.0 f/ ;                          \ 2^-12, exact in f32

: AG-LOSS ( ptr a -- r ) {: y:ptr :}        \ L = sum_i dy[i] * y[i]
   0.0
   AGK 0 ?do  AG-DY i AG-F@  y i AG-F@  f*  f+  loop ;

: AG-NUM-J ( n -- r ) {: jx:n :}
   AG-X jx AG-F@ {: x0:r :}
   x0 AG-EPS f+ AG-X jx AG-F!   AG-X AG-YP AG-FWD-RUN
   x0 AG-EPS f- AG-X jx AG-F!   AG-X AG-YM AG-FWD-RUN
   x0 AG-X jx AG-F!
   AG-YP AG-LOSS AG-YM AG-LOSS f-  AG-EPS 2.0 f* f/ ;

: AG-GRAD ( -- )                             \ fill AG-DXN (numeric) + AG-DXA (analytic)
   AGK 0 ?do  i AG-NUM-J  AG-DXN i AG-F!  loop
   AG-BWD-RUN ;

: AG-TOL ( r -- r )                          \ atol 0.01 + rtol 0.05 * |analytic|
   fabs 0.05 f* 0.01 f+ ;

: AG-MISS-1 ( r r -- n ) {: num:r ana:r :}   \ 1 when |num-ana| exceeds tolerance
   num ana f- fabs  ana AG-TOL  f> if 1 else 0 then ;

: AG-COUNT-MISS ( ptr a ptr a -- n ) {: num:ptr ana:ptr :}
   0 AG-MISS# !
   AGK 0 ?do
      num i AG-F@ ana i AG-F@ AG-MISS-1 AG-MISS# @ + AG-MISS# !
   loop
   AG-MISS# @ ;

: AG-MISMATCH# ( -- n )
   AG-DXN AG-DXA AG-COUNT-MISS ;

\ ---- fixture drivers ------------------------------------------------------------

: AG-X4! ( r r r r -- )
   AG-X 3 AG-F!  AG-X 2 AG-F!  AG-X 1 AG-F!  AG-X 0 AG-F! ;

: AG-DY4! ( r r r r -- )
   AG-DY 3 AG-F!  AG-DY 2 AG-F!  AG-DY 1 AG-F!  AG-DY 0 AG-F! ;

: AG-DY-RND ( -- )
   0.3164 1.0781 0.5977 0.8438 AG-DY4! ;

: AG-CASE ( ptr u8 n -- ) {: la:ptr lu:n :}   \ positive: gradients must AGREE
   AG-GRAD
   AG-MISMATCH# 0 T=
   s" gradcheck PASS on Orin: " type la lu type cr ;

: AG-CASE-WRONG ( ptr u8 n -- ) {: la:ptr lu:n :}   \ negative: gate must DETECT
   AG-GRAD
   AG-MISMATCH# 0 > TTRUE
   s" gradcheck REJECTS on Orin: " type la lu type cr ;

: AG-EXP-ENTRY ( -- )                         \ VJP entry: EXP (da = ct * e)
   s" exp-fwd.cubin" s" exp-bwd.cubin" AG-PAIR!
   AG-DY-RND
   0.7305 -1.4141 1.9023 -0.3672 AG-X4!  s" EXP rnd" AG-CASE
   8.0 -8.0 0.0 3.0 AG-X4!               s" EXP saturated" AG-CASE
   AG-PAIR-CLOSE ;

\ x - max(x): BMAX select + B- both branches + DUP fan-out. No tie fixture BY
\ DESIGN: x - max(x) is kinked at ties, so the central difference straddles the
\ kink and no subgradient can match it there (softmax covers the tie case).
: AG-XMSUB-ENTRY ( -- )
   s" xmsub-fwd.cubin" s" xmsub-bwd.cubin" AG-PAIR!
   AG-DY-RND
   0.7305 -1.4141 1.9023 -0.3672 AG-X4!  s" XMSUB rnd" AG-CASE
   8.0 -8.0 0.0 3.0 AG-X4!               s" XMSUB saturated" AG-CASE
   AG-PAIR-CLOSE ;

: AG-XDIVSUM-ENTRY ( -- )                     \ BSUM broadcast + B/ both branches + fan-out
   s" xdivsum-fwd.cubin" s" xdivsum-bwd.cubin" AG-PAIR!
   AG-DY-RND
   0.5312 2.1484 1.3359 0.8203 AG-X4!    s" XDIVSUM rnd" AG-CASE
   2.0 2.0 1.0 0.5 AG-X4!                s" XDIVSUM tie" AG-CASE
   8.0 0.01 0.5 3.0 AG-X4!               s" XDIVSUM saturated" AG-CASE
   AG-PAIR-CLOSE ;

: AG-SOFTMAX-ENTRY ( -- )                     \ the full generated backward
   s" softmax-fwd.cubin" s" softmax-bwd.cubin" AG-PAIR!
   AG-DY-RND
   0.7305 -1.4141 1.9023 -0.3672 AG-X4!  s" SOFTMAX rnd" AG-CASE
   2.0 2.0 1.0 0.0 AG-X4!                s" SOFTMAX tie" AG-CASE
   8.0 -8.0 0.0 3.0 AG-X4!               s" SOFTMAX saturated" AG-CASE
   AG-PAIR-CLOSE ;

: AG-WRONG-VJP ( -- )                         \ fan-out cotangent dropped -> must mismatch
   s" xdivsum-fwd.cubin" s" xdivsum-bwd-wrong.cubin" AG-PAIR!
   AG-DY-RND
   0.5312 2.1484 1.3359 0.8203 AG-X4!    s" wrong VJP (fan-out dropped)" AG-CASE-WRONG
   AG-PAIR-CLOSE ;

: AG-CROSS-PAIR ( -- )                        \ backward of the WRONG function -> must mismatch
   s" softmax-fwd.cubin" s" xdivsum-bwd.cubin" AG-PAIR!
   AG-DY-RND
   0.7305 -1.4141 1.9023 -0.3672 AG-X4!  s" cross-paired backward" AG-CASE-WRONG
   AG-PAIR-CLOSE ;

\ ==== two-input entries (+. -. *. /. via AD2, OVER/DROP composites) ==============

: AG-Y4! ( r r r r -- )
   AG-Y 3 AG-F!  AG-Y 2 AG-F!  AG-Y 1 AG-F!  AG-Y 0 AG-F! ;

: AG2-UPLOAD-XY ( -- )
   AG-X AG-IN AG-PACK   AG-Y AG-DYB AG-PACK
   AG-dX @ >CUDA-DEVPTR AG-IN AGBYTES >LEN CUDA:CUMEMCPYHTOD CUDA:RC0
   AG-dY2 @ >CUDA-DEVPTR AG-DYB AGBYTES >LEN CUDA:CUMEMCPYHTOD CUDA:RC0 ;

: AG-READBACK ( n ptr a -- ) {: dev:n dst:ptr :}   \ poisoned DtoH into a host array
   AG-POISON
   AG-OUT dev >CUDA-DEVPTR AGBYTES >LEN CUDA:CUMEMCPYDTOH CUDA:RC0
   AG-GUARD
   AG-OUT dst AG-UNPACK ;

: AG2-FWD-RUN ( ptr a -- ) {: dst:ptr :}     \ AD2_FWD(x, y, out, k)
   1 AGK AGBLOCK PTX-ROW-LAUNCH-CHECK
   AG2-UPLOAD-XY
   AG-FWD @ >CUDA-FN AGBLOCK 1 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   AG-FWD @ >CUDA-FN 28 >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   AG-FWD @ >CUDA-FN 0 >IDX  AG-dX 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-FWD @ >CUDA-FN 8 >IDX  AG-dY2 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-FWD @ >CUDA-FN 16 >IDX AG-dO 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-FWD @ >CUDA-FN 24 >IDX AG-KV 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-FWD @ >CUDA-FN 1 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   AG-dO @ dst AG-READBACK ;

: AG2-BWD-RUN ( -- )                         \ AD2_BWD(x, y, dz, dxo, dyo, k)
   1 AGK AGBLOCK PTX-ROW-LAUNCH-CHECK
   AG2-UPLOAD-XY
   AG-DY AG-IN AG-PACK
   AG-dDY @ >CUDA-DEVPTR AG-IN AGBYTES >LEN CUDA:CUMEMCPYHTOD CUDA:RC0
   AG-BWD @ >CUDA-FN AGBLOCK 1 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   AG-BWD @ >CUDA-FN 44 >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   AG-BWD @ >CUDA-FN 0 >IDX  AG-dX 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 8 >IDX  AG-dY2 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 16 >IDX AG-dDY 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 24 >IDX AG-dO 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 32 >IDX AG-dO2 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 40 >IDX AG-KV 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 1 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   AG-dO @ AG-DXA AG-READBACK
   AG-dO2 @ AG-DYA AG-READBACK ;

\ typed-local-lint: allow-bare-local - runner keeps the fwd-run quotation effect.
: AG2-NUM-J ( ptr a n [ ptr a -- ] -- r ) {: arr:ptr jx:n runner :}
   arr jx AG-F@ {: x0:r :}
   x0 AG-EPS f+ arr jx AG-F!   AG-YP runner execute
   x0 AG-EPS f- arr jx AG-F!   AG-YM runner execute
   x0 arr jx AG-F!
   AG-YP AG-LOSS AG-YM AG-LOSS f-  AG-EPS 2.0 f* f/ ;

: AG2-GRAD ( -- )                            \ numeric d/dx + d/dy, then analytic
   AGK 0 ?do  AG-X i [: AG2-FWD-RUN ;] AG2-NUM-J  AG-DXN i AG-F!  loop
   AGK 0 ?do  AG-Y i [: AG2-FWD-RUN ;] AG2-NUM-J  AG-DYN i AG-F!  loop
   AG2-BWD-RUN ;

: AG2-MISS# ( -- n )
   AG-DXN AG-DXA AG-COUNT-MISS
   AG-DYN AG-DYA AG-COUNT-MISS + ;

: AG2-CASE ( ptr u8 n -- ) {: la:ptr lu:n :}
   AG2-GRAD
   AG2-MISS# 0 T=
   s" gradcheck PASS on Orin: " type la lu type cr ;

: AG2-CASE-WRONG ( ptr u8 n -- ) {: la:ptr lu:n :}
   AG2-GRAD
   AG2-MISS# 0 > TTRUE
   s" gradcheck REJECTS on Orin: " type la lu type cr ;

: AG2-RND ( -- )
   AG-DY-RND
   0.7305 -1.4141 1.9023 -0.3672 AG-X4!
   1.2891 0.6172 -0.9453 1.7734 AG-Y4! ;

: AG2-RND-POS ( -- )                          \ y bounded away from 0 for /.
   AG-DY-RND
   0.7305 -1.4141 1.9023 -0.3672 AG-X4!
   1.2891 0.6172 2.0453 0.7734 AG-Y4! ;

: AG-ADD2-ENTRY ( -- )
   s" add2-fwd.cubin" s" add2-bwd.cubin" s" AD2_FWD" s" AD2_BWD" AG2-PAIR!
   AG2-RND  s" +. (dx=dz dy=dz)" AG2-CASE
   AG-PAIR-CLOSE ;

: AG-SUB2-ENTRY ( -- )
   s" sub2-fwd.cubin" s" sub2-bwd.cubin" s" AD2_FWD" s" AD2_BWD" AG2-PAIR!
   AG2-RND  s" -. (dx=dz dy=-dz)" AG2-CASE
   AG-PAIR-CLOSE ;

: AG-MUL2-ENTRY ( -- )
   s" mul2-fwd.cubin" s" mul2-bwd.cubin" s" AD2_FWD" s" AD2_BWD" AG2-PAIR!
   AG2-RND  s" *. (dx=dz*y dy=dz*x)" AG2-CASE
   AG-PAIR-CLOSE ;

: AG-DIV2-ENTRY ( -- )
   s" div2-fwd.cubin" s" div2-bwd.cubin" s" AD2_FWD" s" AD2_BWD" AG2-PAIR!
   AG2-RND-POS  s" /. (dx=dz/y dy=-dz*z/y)" AG2-CASE
   AG-PAIR-CLOSE ;

: AG-OVERK-ENTRY ( -- )                       \ OVER fan-out: SUM both cotangents
   s" overk-fwd.cubin" s" overk-bwd.cubin" s" AD2_FWD" s" AD2_BWD" AG2-PAIR!
   AG2-RND  s" OVER fan-out (z=x*y+x)" AG2-CASE
   AG-PAIR-CLOSE
   s" overk-fwd.cubin" s" overk-bwd-wrong.cubin" s" AD2_FWD" s" AD2_BWD" AG2-PAIR!
   AG2-RND  s" OVER-as-permutation (copy cotangent dropped)" AG2-CASE-WRONG
   AG-PAIR-CLOSE ;

: AG-DROPK-ENTRY ( -- )                       \ DROP: typed zero, never a leak
   s" dropk-fwd.cubin" s" dropk-bwd.cubin" s" AD2_FWD" s" AD2_BWD" AG2-PAIR!
   AG2-RND  s" DROP typed zero (z=x*x, dy=0)" AG2-CASE
   AG-PAIR-CLOSE
   s" dropk-fwd.cubin" s" dropk-bwd-wrong.cubin" s" AD2_FWD" s" AD2_BWD" AG2-PAIR!
   AG2-RND  s" DROP cotangent leak (dy=dz)" AG2-CASE-WRONG
   AG-PAIR-CLOSE ;

\ ==== scalar-factor entries (SCALE via ADS, FMA. via ADF) ========================

: AG-A! ( r -- )
   AG-AH 0 AG-F!
   AG-AH 0 AG-F@ F64>F32 AG-AB ! ;

: AGS-FWD-RUN ( ptr a -- ) {: dst:ptr :}     \ ADS_FWD(x, out, a, k)
   1 AGK AGBLOCK PTX-ROW-LAUNCH-CHECK
   AG-AH 0 AG-F@ F64>F32 AG-AB !
   AG-X AG-IN AG-PACK
   AG-dX @ >CUDA-DEVPTR AG-IN AGBYTES >LEN CUDA:CUMEMCPYHTOD CUDA:RC0
   AG-FWD @ >CUDA-FN AGBLOCK 1 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   AG-FWD @ >CUDA-FN 24 >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   AG-FWD @ >CUDA-FN 0 >IDX  AG-dX 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-FWD @ >CUDA-FN 8 >IDX  AG-dO 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-FWD @ >CUDA-FN 16 >IDX AG-AB 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-FWD @ >CUDA-FN 20 >IDX AG-KV 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-FWD @ >CUDA-FN 1 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   AG-dO @ dst AG-READBACK ;

: AGS-BWD-RUN ( -- )                         \ ADS_BWD(x, dz, dxo, dao, a, k)
   1 AGK AGBLOCK PTX-ROW-LAUNCH-CHECK
   AG-AH 0 AG-F@ F64>F32 AG-AB !
   AG-X AG-IN AG-PACK   AG-DY AG-DYB AG-PACK
   AG-dX @ >CUDA-DEVPTR AG-IN AGBYTES >LEN CUDA:CUMEMCPYHTOD CUDA:RC0
   AG-dDY @ >CUDA-DEVPTR AG-DYB AGBYTES >LEN CUDA:CUMEMCPYHTOD CUDA:RC0
   AG-BWD @ >CUDA-FN AGBLOCK 1 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   AG-BWD @ >CUDA-FN 40 >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   AG-BWD @ >CUDA-FN 0 >IDX  AG-dX 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 8 >IDX  AG-dDY 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 16 >IDX AG-dO 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 24 >IDX AG-dO2 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 32 >IDX AG-AB 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 36 >IDX AG-KV 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 1 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   AG-dO @ AG-DXA AG-READBACK
   AG-dO2 @ AG-DYA AG-READBACK             \ dao broadcast tile; lane 0 = da
   AG-DYA 0 AG-F@ AG-DAA 0 AG-F! ;

\ typed-local-lint: allow-bare-local - runner keeps the fwd-run quotation effect.
: AG-NUM-A ( [ ptr a -- ] -- r ) {: runner :}   \ central difference over the scalar a
   AG-AH 0 AG-F@ {: a0:r :}
   a0 AG-EPS f+ AG-AH 0 AG-F!   AG-YP runner execute
   a0 AG-EPS f- AG-AH 0 AG-F!   AG-YM runner execute
   a0 AG-AH 0 AG-F!
   AG-YP AG-LOSS AG-YM AG-LOSS f-  AG-EPS 2.0 f* f/ ;

: AGS-GRAD ( -- )
   AGK 0 ?do  AG-X i [: AGS-FWD-RUN ;] AG2-NUM-J  AG-DXN i AG-F!  loop
   [: AGS-FWD-RUN ;] AG-NUM-A AG-DAN 0 AG-F!
   AGS-BWD-RUN ;

: AGS-MISS# ( -- n )
   AG-DXN AG-DXA AG-COUNT-MISS
   AG-DAN 0 AG-F@ AG-DAA 0 AG-F@ AG-MISS-1 + ;

: AG-SCALE-ENTRY ( -- )                       \ SCALE: dx=a*dz, da=Sum(dz*x)
   s" scale-fwd.cubin" s" scale-bwd.cubin" s" ADS_FWD" s" ADS_BWD" AG2-PAIR!
   AG-DY-RND
   0.7305 -1.4141 1.9023 -0.3672 AG-X4!
   1.7 AG-A!
   AGS-GRAD
   AGS-MISS# 0 T=
   s" gradcheck PASS on Orin: SCALE (dx=a*dz da=Sum(dz*x))" type cr
   AG-PAIR-CLOSE ;

: AGF-FWD-RUN ( ptr a -- ) {: dst:ptr :}     \ ADF_FWD(x, y, out, a, k)
   1 AGK AGBLOCK PTX-ROW-LAUNCH-CHECK
   AG-AH 0 AG-F@ F64>F32 AG-AB !
   AG2-UPLOAD-XY
   AG-FWD @ >CUDA-FN AGBLOCK 1 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   AG-FWD @ >CUDA-FN 32 >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   AG-FWD @ >CUDA-FN 0 >IDX  AG-dX 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-FWD @ >CUDA-FN 8 >IDX  AG-dY2 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-FWD @ >CUDA-FN 16 >IDX AG-dO 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-FWD @ >CUDA-FN 24 >IDX AG-AB 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-FWD @ >CUDA-FN 28 >IDX AG-KV 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-FWD @ >CUDA-FN 1 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   AG-dO @ dst AG-READBACK ;

: AGF-BWD-RUN ( -- )                         \ ADF_BWD(x, dz, dxo, dyo, dao, a, k)
   1 AGK AGBLOCK PTX-ROW-LAUNCH-CHECK
   AG-AH 0 AG-F@ F64>F32 AG-AB !
   AG-X AG-IN AG-PACK   AG-DY AG-DYB AG-PACK
   AG-dX @ >CUDA-DEVPTR AG-IN AGBYTES >LEN CUDA:CUMEMCPYHTOD CUDA:RC0
   AG-dDY @ >CUDA-DEVPTR AG-DYB AGBYTES >LEN CUDA:CUMEMCPYHTOD CUDA:RC0
   AG-BWD @ >CUDA-FN AGBLOCK 1 1 CUDA:CUFUNCSETBLOCKSHAPE CUDA:RC0
   AG-BWD @ >CUDA-FN 48 >LEN CUDA:CUPARAMSETSIZE CUDA:RC0
   AG-BWD @ >CUDA-FN 0 >IDX  AG-dX 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 8 >IDX  AG-dDY 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 16 >IDX AG-dO 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 24 >IDX AG-dO2 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 32 >IDX AG-dO3 8 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 40 >IDX AG-AB 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 44 >IDX AG-KV 4 >LEN CUDA:CUPARAMSETV CUDA:RC0
   AG-BWD @ >CUDA-FN 1 1 CUDA:CULAUNCHGRID CUDA:RC0
   CUDA:CUCTXSYNCHRONIZE CUDA:RC0
   AG-dO @ AG-DXA AG-READBACK
   AG-dO2 @ AG-DYA AG-READBACK
   AG-dO3 @ AG-YP AG-READBACK              \ dao broadcast tile via scratch
   AG-YP 0 AG-F@ AG-DAA 0 AG-F! ;

: AGF-GRAD ( -- )
   AGK 0 ?do  AG-X i [: AGF-FWD-RUN ;] AG2-NUM-J  AG-DXN i AG-F!  loop
   AGK 0 ?do  AG-Y i [: AGF-FWD-RUN ;] AG2-NUM-J  AG-DYN i AG-F!  loop
   [: AGF-FWD-RUN ;] AG-NUM-A AG-DAN 0 AG-F!
   AGF-BWD-RUN ;

: AGF-MISS# ( -- n )
   AG2-MISS#
   AG-DAN 0 AG-F@ AG-DAA 0 AG-F@ AG-MISS-1 + ;

: AG-FMA-ENTRY ( -- )                         \ FMA.: da=Sum(dz*x), dx=a*dz, dy=dz
   s" fma-fwd.cubin" s" fma-bwd.cubin" s" ADF_FWD" s" ADF_BWD" AG2-PAIR!
   AG2-RND
   0.6 AG-A!
   AGF-GRAD
   AGF-MISS# 0 T=
   s" gradcheck PASS on Orin: FMA. (da=Sum(dz*x) dx=a*dz dy=dz)" type cr
   AG-PAIR-CLOSE ;

\ ---- entry ----------------------------------------------------------------------

: AG-DIR! ( -- )
   SCRIPT-ARGC 1 <> if s" usage: ad-gradcheck-launch.f -- <cubin-dir>" 64 die then
   0 SCRIPT-ARGV$ {: a:ptr u:n :}
   u 255 > if s" ad-gradcheck: dir path too long" 64 die then
   a AG-DIR u BYTE-COPY
   u AG-DIR-U ! ;

: AG-MAIN ( -- )
   T-RESET
   CUDA:OPEN? 0= if s" ad-gradcheck: no libcuda, skipping (not a device box)" type cr T-REPORT exit then
   AG-DIR!
   AG-INIT
   AG-EXP-ENTRY
   AG-XMSUB-ENTRY
   AG-XDIVSUM-ENTRY
   AG-SOFTMAX-ENTRY
   AG-WRONG-VJP
   AG-CROSS-PAIR
   AG-ADD2-ENTRY
   AG-SUB2-ENTRY
   AG-MUL2-ENTRY
   AG-DIV2-ENTRY
   AG-OVERK-ENTRY
   AG-DROPK-ENTRY
   AG-SCALE-ENTRY
   AG-FMA-ENTRY
   AG-FINI
   T-REPORT ;

AG-MAIN
