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
create AG-DY 4 cells allot         \ host f64: output cotangent
create AG-YP 4 cells allot         \ host f64: forward at x+h
create AG-YM 4 cells allot         \ host f64: forward at x-h
create AG-DXN 4 cells allot        \ host f64: numeric gradient
create AG-DXA 4 cells allot        \ host f64: analytic gradient

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
   AGK AG-KV ! ;

: AG-FINI ( -- )
   AG-dX @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   AG-dDY @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   AG-dO @ >CUDA-DEVPTR CUDA:CUMEMFREE CUDA:RC0
   AG-DEV @ >CUDA-DEV CUDA:CUDEVICEPRIMARYCTXRELEASE CUDA:RC0 ;

: AG-PAIR! ( ptr u8 n ptr u8 n -- ) {: f:ptr fu:n b:ptr bu:n :}
   f fu AG-PATH!
   AG-MF AG-PZ CUDA:CUMODULELOAD CUDA:RC0
   s" AD_FWD" AG-KN >CSTR
   AG-FWD AG-MF @ >CUDA-MOD AG-KN CUDA:CUMODULEGETFUNCTION CUDA:RC0
   b bu AG-PATH!
   AG-MB AG-PZ CUDA:CUMODULELOAD CUDA:RC0
   s" AD_BWD" AG-KN >CSTR
   AG-BWD AG-MB @ >CUDA-MOD AG-KN CUDA:CUMODULEGETFUNCTION CUDA:RC0 ;

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

: AG-TOL ( r -- r )                          \ atol 0.01 + rtol 0.05 * |dxa|
   fabs 0.05 f* 0.01 f+ ;

: AG-MISMATCH# ( -- n )
   0 AG-MISS# !
   AGK 0 ?do
      AG-DXN i AG-F@ AG-DXA i AG-F@ f- fabs
      AG-DXA i AG-F@ AG-TOL
      f> if AG-MISS# @ 1+ AG-MISS# ! then
   loop
   AG-MISS# @ ;

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
   AG-FINI
   T-REPORT ;

AG-MAIN
