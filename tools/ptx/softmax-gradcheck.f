\ softmax-gradcheck.f - device finite-difference gradcheck of the AUTO-DERIVED
\ SOFTMAX-ROWS backward (lib/ptx/ad-dag.f -> tools/ptx/softmax-bwd-cg.f).
\
\ Runs the AD-emitted backward kernel (x,dy -> dx) on the Orin, then independently
\ forms the numerical gradient by central differences: perturb each x[j] by +-eps,
\ re-run the forward SOFTMAX-ROWS, and accumulate sum_i dy[i]*(y+[i]-y-[i])/(2eps).
\ Both use the SAME ex2.approx forward, so they agree to finite-diff + f32 error.
\ Fully checked Habu via lib/ffi.f. Self-contained: spawns bin/hb to emit ONE PTX
\ module holding BOTH the forward SOFTMAX_ROWS and the AD-derived SOFTMAX_BWD kernels
\ (tools/ptx/softmax-fb-cg.f), ptxas-assembles it to a PRIVATE per-run cubin under a
\ toolchain root, loads that SINGLE cubin, and pulls BOTH function handles from it -
\ no shared /tmp/softmax.cubin + /tmp/softmax-bwd.cubin pair. Load after lib/errors.f
\ lib/string.f lib/test.f lib/float.f lib/fmt.f src/arch/ptx/emit.f lib/ptx/cg.f
\ lib/ptx/header.f lib/ptx/launch.f lib/ffi.f maki/array.f.

require lib/ptx/toolchain.f
require lib/ptx/sentinel.f

4 constant GCK
create GC-LIB 16 allot  create GC-NM 64 allot  create GC-P1 64 allot
create GC-KF 32 allot   create GC-KB 32 allot
create GC-IN 16 allot   create GC-OUT 16 allot   create GC-DYB 16 allot    \ f32 device-side packs
create GC-EOUT $8000 allot  create GC-EERR $1000 allot                     \ child emit capture
create GC-QO $1000 allot    create GC-QE $1000 allot                       \ ptxas capture
create HX 4 cells allot create HDY 4 cells allot  create HDXA 4 cells allot \ host f64
create HDXN 4 cells allot
create HYP 4 cells allot create HYM 4 cells allot
variable GC-H variable GC-DEV variable GC-CTX variable GC-MF
variable GC-FWD variable GC-BWD variable GC-dX variable GC-dDY variable GC-dO variable GC-KV

: F32! ( n ptr u8 n -- ) {: v buf idx :} idx 4 * {: o :}
   v $FF and buf o + c!  v 8 rshift $FF and buf o 1 + + c!
   v 16 rshift $FF and buf o 2 + + c!  v 24 rshift $FF and buf o 3 + + c! ;
: F32@ ( ptr u8 n -- n ) {: buf idx :} idx 4 * {: o :}
   buf o + c@  buf o 1 + + c@ 8 lshift or  buf o 2 + + c@ 16 lshift or  buf o 3 + + c@ 24 lshift or ;
: PACK4   ( ptr a ptr u8 -- ) {: src dst :}  GCK 0 ?do  src i T-GET F64>F32  dst i F32!  loop ;
: UNPACK4 ( ptr u8 ptr a -- ) {: src dst :}  GCK 0 ?do  src i F32@ F32>F64  dst i T-SET  loop ;
: GC-OUT-GUARD ( -- )  GCK 0 ?do  GC-OUT i F32@ PTXSENT:GUARD drop  loop ;  \ fail closed if the copy-back was dropped

: GC-SYM ( ptr u8 n -- n )  GC-NM >CSTR  GC-H @ GC-NM DLSYM ;

\ libcuda handle (0 iff off-device); a 0 handle means Mac/CI, so the gradcheck skips
: GC-DEVICE? ( -- n )
   s" libcuda.so.1" GC-LIB >CSTR  GC-LIB RTLD-NOW DLOPEN dup GC-H ! ;

\ spawn bin/hb to emit the combined fwd+bwd module (softmax-fb-cg.f) to the private PTX
: GC-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"                    >LEN PROC-ARGV+
   s" tools/ptx/softmax-fb-cg.f" >LEN PROC-ARGV+
   s" bin/hb" >LEN  GC-EOUT $8000 >LEN  GC-EERR $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   GC-EERR erru LEN>N  rc RC>N  PTXTC:EMIT-GUARD           \ nonzero emit rc -> surface stderr, throw
   PTXTC:PTX$ GC-EOUT outu LEN>N WRITE-ALL  outu LEN>N ;

: GC-PTXAS ( -- n )
   GC-QO $1000 >LEN GC-QE $1000 >LEN PTXTC:ASSEMBLE ;

\ load the ONE combined cubin and pull BOTH function handles from the SAME module
: GC-SETUP ( -- )
   0 s" cuInit" GC-SYM CALL1 drop
   GC-DEV P>N 0 s" cuDeviceGet" GC-SYM CALL2 drop
   GC-CTX P>N GC-DEV @ s" cuDevicePrimaryCtxRetain" GC-SYM CALL2 drop
   GC-CTX @ s" cuCtxSetCurrent" GC-SYM CALL1 drop
   PTXTC:CUBIN$ GC-P1 >CSTR
   GC-MF P>N GC-P1 P>N s" cuModuleLoad" GC-SYM CALL2 drop
   s" SOFTMAX_ROWS" GC-KF >CSTR
   GC-FWD P>N GC-MF @ GC-KF P>N s" cuModuleGetFunction" GC-SYM CALL3 drop
   s" SOFTMAX_BWD" GC-KB >CSTR
   GC-BWD P>N GC-MF @ GC-KB P>N s" cuModuleGetFunction" GC-SYM CALL3 drop  \ same module, second entry
   GC-dX P>N 16 s" cuMemAlloc_v2" GC-SYM CALL2 drop
   GC-dDY P>N 16 s" cuMemAlloc_v2" GC-SYM CALL2 drop
   GC-dO P>N 16 s" cuMemAlloc_v2" GC-SYM CALL2 drop
   GCK GC-KV ! ;

\ run the forward softmax on the f64 input array `src`, write the f64 output to `dst`
: GC-FWD-RUN ( ptr a ptr a -- ) {: src dst :}
   GC-OUT 16 PTXSENT:FILL                            \ poison readback: dropped copy-back fails closed
   1 GCK 256 PTX-ROW-LAUNCH-CHECK
   src GC-IN PACK4
   GC-dX @ GC-IN P>N 16 s" cuMemcpyHtoD_v2" GC-SYM CALL3 drop
   GC-FWD @ 256 1 1 s" cuFuncSetBlockShape" GC-SYM CALL4 drop
   GC-FWD @ 20 s" cuParamSetSize" GC-SYM CALL2 drop
   GC-FWD @ 0  GC-dX P>N 8 s" cuParamSetv" GC-SYM CALL4 drop
   GC-FWD @ 8  GC-dO P>N 8 s" cuParamSetv" GC-SYM CALL4 drop
   GC-FWD @ 16 GC-KV P>N 4 s" cuParamSetv" GC-SYM CALL4 drop
   GC-FWD @ 1 1 s" cuLaunchGrid" GC-SYM CALL3 drop
   0 s" cuCtxSynchronize" GC-SYM CALL1 drop
   GC-OUT P>N GC-dO @ 16 s" cuMemcpyDtoH_v2" GC-SYM CALL3 drop
   GC-OUT-GUARD
   GC-OUT dst UNPACK4 ;

\ run the AUTO-DERIVED backward: (HX, HDY) -> HDXA
: GC-BWD-RUN ( -- )
   GC-OUT 16 PTXSENT:FILL                            \ poison readback: dropped copy-back fails closed
   1 GCK 256 PTX-ROW-LAUNCH-CHECK
   HX GC-IN PACK4   HDY GC-DYB PACK4
   GC-dX @ GC-IN P>N 16 s" cuMemcpyHtoD_v2" GC-SYM CALL3 drop
   GC-dDY @ GC-DYB P>N 16 s" cuMemcpyHtoD_v2" GC-SYM CALL3 drop
   GC-BWD @ 256 1 1 s" cuFuncSetBlockShape" GC-SYM CALL4 drop
   GC-BWD @ 28 s" cuParamSetSize" GC-SYM CALL2 drop
   GC-BWD @ 0  GC-dX P>N 8 s" cuParamSetv" GC-SYM CALL4 drop
   GC-BWD @ 8  GC-dDY P>N 8 s" cuParamSetv" GC-SYM CALL4 drop
   GC-BWD @ 16 GC-dO P>N 8 s" cuParamSetv" GC-SYM CALL4 drop
   GC-BWD @ 24 GC-KV P>N 4 s" cuParamSetv" GC-SYM CALL4 drop
   GC-BWD @ 1 1 s" cuLaunchGrid" GC-SYM CALL3 drop
   0 s" cuCtxSynchronize" GC-SYM CALL1 drop
   GC-OUT P>N GC-dO @ 16 s" cuMemcpyDtoH_v2" GC-SYM CALL3 drop
   GC-OUT-GUARD
   GC-OUT HDXA UNPACK4 ;

: GC-RELEASE ( -- )
   GC-MF @ s" cuModuleUnload" GC-SYM CALL1 drop
   GC-DEV @ s" cuDevicePrimaryCtxRelease" GC-SYM CALL1 drop ;

\ numerical dx[j] = sum_i dy[i]*(y+[i]-y-[i]) / (2 eps)
: GC-EPS ( -- r )  1.0 4096.0 f/ ;                  \ 2^-12, exact f32
: GC-NUM-J ( n -- r ) {: jx :}
   HX jx T-GET {: x0 :}
   x0 GC-EPS f+ HX jx T-SET   HX HYP GC-FWD-RUN
   x0 GC-EPS f- HX jx T-SET   HX HYM GC-FWD-RUN
   x0 HX jx T-SET
   0.0  GCK 0 ?do  HDY i T-GET  HYP i T-GET HYM i T-GET f-  f*  f+  loop
   GC-EPS 2.0 f* f/ ;

\ x = [1, 2, 0.5, 1.5], dy = [0.1, 0.2, 0.3, 0.4]   (?do is compile-only -> a word)
: GC-RUN ( -- )
   1.0 HX 0 T-SET  2.0 HX 1 T-SET  0.5 HX 2 T-SET  1.5 HX 3 T-SET
   0.1 HDY 0 T-SET 0.2 HDY 1 T-SET 0.3 HDY 2 T-SET 0.4 HDY 3 T-SET
   GC-SETUP
   GCK 0 ?do  i GC-NUM-J  HDXN i T-SET  loop          \ numerical gradient (forwards)
   GC-BWD-RUN                                         \ analytic gradient -> HDXA (backward last)
   GC-RELEASE
   GCK 0 ?do  HDXN i T-GET  HDXA i T-GET  f-  fabs  1.0 100.0 f/ f<  TTRUE  loop ;

\ self-emit the combined module, assemble to one private cubin, gradcheck, clean up.
\ Off-device (no libcuda) the gradcheck skips fail-closed with an empty report.
: SOFTMAX-GRADCHECK-MAIN ( -- )
   T-RESET
   GC-DEVICE? 0= if T-REPORT exit then
   s" habu-ptx-softmax-fb" PTXTC:PREPARE
   GC-EMIT drop
   GC-PTXAS PTXTC:ASM-REPORT 0 T=                  \ surface ptxas stderr before the assert
   GC-RUN
   PTXTC:CLEAN
   T-REPORT ;

SOFTMAX-GRADCHECK-MAIN
