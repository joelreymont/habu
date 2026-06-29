\ softmax-gradcheck.f - device finite-difference gradcheck of the AUTO-DERIVED
\ SOFTMAX-ROWS backward (lib/ptx/ad-dag.f -> tools/ptx/softmax-bwd-cg.f).
\
\ Runs the AD-emitted backward kernel (x,dy -> dx) on the Orin, then independently
\ forms the numerical gradient by central differences: perturb each x[j] by +-eps,
\ re-run the forward SOFTMAX-ROWS, and accumulate sum_i dy[i]*(y+[i]-y-[i])/(2eps).
\ Both use the SAME ex2.approx forward, so they agree to finite-diff + f32 error.
\ Fully checked Habu via lib/ffi.f. Prereqs: /tmp/softmax.cubin, /tmp/softmax-bwd.cubin.
\ Load after lib/errors.f lib/string.f lib/test.f lib/float.f lib/fmt.f
\ src/arch/ptx/emit.f lib/ptx/cg.f lib/ptx/header.f lib/ptx/launch.f
\ lib/ffi.f maki/array.f.

4 constant GCK
create GC-LIB 16 allot  create GC-NM 64 allot  create GC-P1 64 allot  create GC-P2 64 allot
create GC-KF 32 allot   create GC-KB 32 allot
create GC-IN 16 allot   create GC-OUT 16 allot   create GC-DYB 16 allot    \ f32 device-side packs
create HX 4 cells allot create HDY 4 cells allot  create HDXA 4 cells allot \ host f64
create HDXN 4 cells allot
create HYP 4 cells allot create HYM 4 cells allot
variable GC-H variable GC-DEV variable GC-CTX variable GC-MF variable GC-MB
variable GC-FWD variable GC-BWD variable GC-dX variable GC-dDY variable GC-dO variable GC-KV

: F32! ( n ptr u8 n -- ) {: v buf idx :} idx 4 * {: o :}
   v $FF and buf o + c!  v 8 rshift $FF and buf o 1 + + c!
   v 16 rshift $FF and buf o 2 + + c!  v 24 rshift $FF and buf o 3 + + c! ;
: F32@ ( ptr u8 n -- n ) {: buf idx :} idx 4 * {: o :}
   buf o + c@  buf o 1 + + c@ 8 lshift or  buf o 2 + + c@ 16 lshift or  buf o 3 + + c@ 24 lshift or ;
: PACK4   ( ptr a ptr u8 -- ) {: src dst :}  GCK 0 ?do  src i T-GET F64>F32  dst i F32!  loop ;
: UNPACK4 ( ptr u8 ptr a -- ) {: src dst :}  GCK 0 ?do  src i F32@ F32>F64  dst i T-SET  loop ;

: GC-SYM ( ptr u8 n -- n )  GC-NM >CSTR  GC-H @ GC-NM DLSYM ;

: GC-SETUP ( -- )
   s" libcuda.so.1" GC-LIB >CSTR  GC-LIB RTLD-NOW DLOPEN GC-H !
   0 s" cuInit" GC-SYM CALL1 drop
   GC-DEV P>N 0 s" cuDeviceGet" GC-SYM CALL2 drop
   GC-CTX P>N GC-DEV @ s" cuDevicePrimaryCtxRetain" GC-SYM CALL2 drop
   GC-CTX @ s" cuCtxSetCurrent" GC-SYM CALL1 drop
   s" /tmp/softmax.cubin" GC-P1 >CSTR
   GC-MF P>N GC-P1 P>N s" cuModuleLoad" GC-SYM CALL2 drop
   s" SOFTMAX_ROWS" GC-KF >CSTR
   GC-FWD P>N GC-MF @ GC-KF P>N s" cuModuleGetFunction" GC-SYM CALL3 drop
   s" /tmp/softmax-bwd.cubin" GC-P2 >CSTR
   GC-MB P>N GC-P2 P>N s" cuModuleLoad" GC-SYM CALL2 drop
   s" SOFTMAX_BWD" GC-KB >CSTR
   GC-BWD P>N GC-MB @ GC-KB P>N s" cuModuleGetFunction" GC-SYM CALL3 drop
   GC-dX P>N 16 s" cuMemAlloc_v2" GC-SYM CALL2 drop
   GC-dDY P>N 16 s" cuMemAlloc_v2" GC-SYM CALL2 drop
   GC-dO P>N 16 s" cuMemAlloc_v2" GC-SYM CALL2 drop
   GCK GC-KV ! ;

\ run the forward softmax on the f64 input array `src`, write the f64 output to `dst`
: GC-FWD-RUN ( ptr a ptr a -- ) {: src dst :}
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
   GC-OUT dst UNPACK4 ;

\ run the AUTO-DERIVED backward: (HX, HDY) -> HDXA
: GC-BWD-RUN ( -- )
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
   GC-OUT HDXA UNPACK4 ;

: GC-RELEASE ( -- )
   GC-MF @ s" cuModuleUnload" GC-SYM CALL1 drop
   GC-MB @ s" cuModuleUnload" GC-SYM CALL1 drop
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

T-RESET
GC-RUN
T-REPORT
bye
