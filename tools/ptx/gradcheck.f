\ gradcheck.f - device-run central-difference gradcheck (the AD "hard gate" v0).
\
\ The checker proves TYPES, not the DERIVATIVE: a wrong VJP type-checks and ships a
\ silently wrong gradient. This harness closes that gap NUMERICALLY on the Orin: launch a
\ forward kernel at x+h and x-h, form the central difference (f(x+h)-f(x-h))/2h, and
\ compare it to the analytic VJP. It gates two ops - SAXPY (LINEAR: z=a*x+y, dz/dx=a=3) and
\ RELU (NONLINEAR with a kink: dz/dx=1 for x>0, 0 for x<0) - and shows a WRONG analytic VJP
\ is rejected. This is the gate over the VJP table / generated backwards (extension to the
\ auto-derived softmax backward is dotted habu-ad-thread-saved + habu-ad-softmax-rows).
\
\ The primary context is retained ONCE (GC-CTX-INIT) and released ONCE (GC-CTX-FINI); a
\ retained context never released hangs bin/hb at process exit on the Orin (RCA
\ habu-rca-device-gradcheck). Self-contained, Orin-only. Load after lib/test.f, lib/ffi.f,
\ lib/ptx/cg.f (F32>F64/F64>F32), and the fs/process libs.

require lib/ptx/toolchain.f

create GC-LIB 16 allot  create GC-NM 64 allot  create GC-PATH 64 allot  create GC-KN 32 allot
variable GC-H variable GC-DEV variable GC-CTX variable GC-MOD variable GC-FUNC
variable GC-DX variable GC-DY variable GC-AB variable GC-NV variable GC-RBUF
create GC-OUT $4000 allot  create GC-ERR $1000 allot
create GC-QOUT $1000 allot create GC-QERR $1000 allot

: GC-SYM ( ptr u8 n -- n )  GC-NM >CSTR  GC-H @ GC-NM DLSYM ;

: GC-PRELUDE ( -- )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"     >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f"  >LEN PROC-ARGV+
   s" lib/ptx/header.f"     >LEN PROC-ARGV+  s" lib/ptx/cg-collective.f" >LEN PROC-ARGV+
   s" lib/ptx/tile.f"       >LEN PROC-ARGV+
   s" lib/ptx/collective.f" >LEN PROC-ARGV+ ;
: GC-RUN-EMIT ( -- n )
   s" bin/hb" >LEN  GC-OUT $4000 >LEN  GC-ERR $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}
   PTXTC:PTX$ GC-OUT outu LEN>N WRITE-ALL  outu LEN>N ;
: GC-EMIT-SAXPY ( -- n )  GC-PRELUDE  s" tools/ptx/saxpy-cg.f" >LEN PROC-ARGV+  GC-RUN-EMIT ;
: GC-EMIT-RELU  ( -- n )  GC-PRELUDE  s" tools/ptx/relu-cg.f"  >LEN PROC-ARGV+  GC-RUN-EMIT ;
: GC-EMIT-EXP   ( -- n )  GC-PRELUDE  s" tools/ptx/exp-cg.f"   >LEN PROC-ARGV+  GC-RUN-EMIT ;
: GC-EMIT-EXPBWD ( -- n ) GC-PRELUDE  s" tools/ptx/expbwd-cg.f" >LEN PROC-ARGV+  GC-RUN-EMIT ;

: GC-PTXAS ( -- n )
   GC-QOUT $1000 >LEN GC-QERR $1000 >LEN PTXTC:ASSEMBLE ;

: GC-CTX-INIT ( -- )
   s" libcuda.so.1" GC-LIB >CSTR  GC-LIB RTLD-NOW DLOPEN GC-H !
   0                      s" cuInit"                    GC-SYM CALL1 drop
   GC-DEV P>N 0           s" cuDeviceGet"               GC-SYM CALL2 drop
   GC-CTX P>N GC-DEV @    s" cuDevicePrimaryCtxRetain"  GC-SYM CALL2 drop
   GC-CTX @              s" cuCtxSetCurrent"           GC-SYM CALL1 drop
   GC-DX P>N 16          s" cuMemAlloc_v2"   GC-SYM CALL2 drop
   GC-DY P>N 16          s" cuMemAlloc_v2"   GC-SYM CALL2 drop ;
: GC-LOAD ( -- )
   PTXTC:CUBIN$ GC-PATH >CSTR
   GC-MOD P>N GC-PATH P>N s" cuModuleLoad"              GC-SYM CALL2 drop
   s" SAXPY" GC-KN >CSTR
   GC-FUNC P>N GC-MOD @ GC-KN P>N s" cuModuleGetFunction" GC-SYM CALL3 drop ;
: GC-UNLOAD ( -- )  GC-MOD @ s" cuModuleUnload" GC-SYM CALL1 drop ;
: GC-CTX-FINI ( -- )  GC-DEV @ s" cuDevicePrimaryCtxRelease" GC-SYM CALL1 drop ;

\ launch the loaded kernel at x = xbits (a=3.0, y=0, n=1) -> z[0] f32 bits
: GC-AT ( n -- n ) {: xbits :}
   GC-DX @ xbits 1        s" cuMemsetD32_v2"  GC-SYM CALL3 drop
   GC-DY @ 0 1            s" cuMemsetD32_v2"  GC-SYM CALL3 drop
   $40400000 GC-AB !  1 GC-NV !
   GC-FUNC @ 256 1 1      s" cuFuncSetBlockShape" GC-SYM CALL4 drop
   GC-FUNC @ 24           s" cuParamSetSize"  GC-SYM CALL2 drop
   GC-FUNC @ 0  GC-DX P>N 8  s" cuParamSetv"  GC-SYM CALL4 drop
   GC-FUNC @ 8  GC-DY P>N 8  s" cuParamSetv"  GC-SYM CALL4 drop
   GC-FUNC @ 16 GC-AB P>N 4  s" cuParamSetv"  GC-SYM CALL4 drop
   GC-FUNC @ 20 GC-NV P>N 4  s" cuParamSetv"  GC-SYM CALL4 drop
   GC-FUNC @ 1 1          s" cuLaunchGrid"    GC-SYM CALL3 drop
   0                      s" cuCtxSynchronize" GC-SYM CALL1 drop
   GC-RBUF P>N GC-DY @ 4  s" cuMemcpyDtoH_v2" GC-SYM CALL3 drop
   GC-RBUF @ $FFFFFFFF and ;

\ launch a 2-input backward kernel: x=dz, y=savedy (n=1) -> result in x[0] f32 bits
: GC-AT-2IN ( n n -- n ) {: dzbits sybits :}
   GC-DX @ dzbits 1       s" cuMemsetD32_v2"  GC-SYM CALL3 drop      \ dz
   GC-DY @ sybits 1       s" cuMemsetD32_v2"  GC-SYM CALL3 drop      \ savedy
   $40400000 GC-AB !  1 GC-NV !
   GC-FUNC @ 256 1 1      s" cuFuncSetBlockShape" GC-SYM CALL4 drop
   GC-FUNC @ 24           s" cuParamSetSize"  GC-SYM CALL2 drop
   GC-FUNC @ 0  GC-DX P>N 8  s" cuParamSetv"  GC-SYM CALL4 drop
   GC-FUNC @ 8  GC-DY P>N 8  s" cuParamSetv"  GC-SYM CALL4 drop
   GC-FUNC @ 16 GC-AB P>N 4  s" cuParamSetv"  GC-SYM CALL4 drop
   GC-FUNC @ 20 GC-NV P>N 4  s" cuParamSetv"  GC-SYM CALL4 drop
   GC-FUNC @ 1 1          s" cuLaunchGrid"    GC-SYM CALL3 drop
   0                      s" cuCtxSynchronize" GC-SYM CALL1 drop
   GC-RBUF P>N GC-DX @ 4  s" cuMemcpyDtoH_v2" GC-SYM CALL3 drop      \ backward output in dz (x)
   GC-RBUF @ $FFFFFFFF and ;

\ central difference of the loaded kernel w.r.t. x at x0, step h -> a Habu float
: GC-CENTRAL ( r r -- r ) {: x0 h :}
   x0 h f+ F64>F32 GC-AT F32>F64 {: zp :}
   x0 h f- F64>F32 GC-AT F32>F64 {: zm :}
   zp zm f-  h 2.0 f* f/ ;

: GC-NEAR? ( r r -- bool ) {: a b :}  a b f- {: d :}  d 0.0 f< if 0.0 d f- else d then  0.05 f< ;

: GRADCHECK-MAIN ( -- )
   T-RESET
   s" habu-ptx-gradcheck" PTXTC:PREPARE
   GC-CTX-INIT
   \ --- SAXPY (linear): d(a*x)/dx = a = 3.0 ---
   GC-EMIT-SAXPY drop  GC-PTXAS 0 T=  GC-LOAD
   2.0 0.001 GC-CENTRAL {: gs :}
   GC-UNLOAD
   \ --- RELU (nonlinear): d max(x,0)/dx = 1 (x>0), 0 (x<0) ---
   GC-EMIT-RELU drop   GC-PTXAS 0 T=  GC-LOAD
   2.0 0.001 GC-CENTRAL {: gp :}                       \ at x=+2
   -2.0 0.001 GC-CENTRAL {: gm :}                      \ at x=-2
   GC-UNLOAD
   \ --- EXP (transcendental): d exp(x)/dx = exp(x) = the forward value (non-constant) ---
   GC-EMIT-EXP drop    GC-PTXAS 0 T=  GC-LOAD
   1.0 0.001 GC-CENTRAL {: ge :}                       \ numeric d exp/dx at x=1
   1.0 F64>F32 GC-AT F32>F64 {: ey :}                  \ exp(1) = the analytic gradient
   GC-UNLOAD
   \ --- EXP BACKWARD KERNEL (resolved SAVED-Y): dx = dz * savedy, run on device ---
   GC-EMIT-EXPBWD drop GC-PTXAS 0 T=  GC-LOAD
   $3F800000 ey F64>F32 GC-AT-2IN F32>F64 {: gb :}     \ backward(dz=1.0, savedy=exp(1)) = exp(1)
   GC-UNLOAD
   GC-CTX-FINI                                         \ release BEFORE exit
   PTXTC:CLEAN
   gs 3.0 GC-NEAR? TTRUE                               \ SAXPY: correct dx=a=3 -> PASS
   gs 2.0 GC-NEAR? TFALSE                              \ SAXPY: wrong dx=2 -> REJECTED
   gp 1.0 GC-NEAR? TTRUE                               \ RELU x>0: dx=1 -> PASS
   gm 0.0 GC-NEAR? TTRUE                               \ RELU x<0: dx=0 -> PASS
   gm 1.0 GC-NEAR? TFALSE                              \ RELU x<0: wrong dx=1 -> REJECTED
   ge ey GC-NEAR? TTRUE                                \ EXP: d exp/dx = exp(x) -> PASS
   ge 1.0 GC-NEAR? TFALSE                              \ EXP: wrong constant dx=1 -> REJECTED
   gb ge GC-NEAR? TTRUE                                \ EXP BACKWARD KERNEL output = numeric gradient
   s" device gradcheck: SAXPY/RELU/EXP forward gradients AND the resolved EXP backward KERNEL (dz*savedy) verified on the Orin" type cr
   T-REPORT ;

GRADCHECK-MAIN
