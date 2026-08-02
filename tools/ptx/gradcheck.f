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
\ The primary context is retained ONCE (GC-CTX-INIT) and released ONCE by the run scope
\ (CUDA-SCOPE); a retained context never released hangs bin/hb at process exit on the Orin (RCA
\ habu-rca-device-gradcheck). Self-contained, Orin-only. Load after lib/test.f, lib/ffi-abi.f,
\ lib/float32.f (F32:WIDEN/F32:NARROW), and the fs/process libs.

require lib/ptx/toolchain.f
require lib/float32.f
require lib/ptx/sentinel.f
require lib/ptx/cuda-driver.f
require lib/ptx/cuda-scope.f
require maki/eval/active-target.f

package GRADCHECK

create GC-PATH 64 allot  create GC-KN 32 allot
variable GC-DEV variable GC-CTX variable GC-MOD variable GC-FUNC
variable GC-DX variable GC-DY variable GC-AB variable GC-NV variable GC-RBUF
create GC-OUT $4000 allot  create GC-ERR $1000 allot
create GC-QOUT $1000 allot create GC-QERR $1000 allot

: GC-PRELUDE ( -- )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"     >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f"  >LEN PROC-ARGV+
   s" lib/ptx/header.f"     >LEN PROC-ARGV+  s" lib/ptx/cg-collective.f" >LEN PROC-ARGV+
   s" lib/ptx/tile.f"       >LEN PROC-ARGV+
   s" lib/ptx/collective.f" >LEN PROC-ARGV+ ;
: GC-WRITE-PTX ( len len -- n )  drop {: o:len :}       \ err-len unused, rc ignored (Shape C)
   PTXTC:PTX$ GC-OUT o LEN>N WRITE-ALL  o LEN>N ;
: GC-RUN-EMIT ( -- n )
   s" bin/hb" >LEN  GC-OUT $4000 >LEN  GC-ERR $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE GC-WRITE-PTX ENDOF
     err OF PCAP-FAILED:UNMAKE drop GC-WRITE-PTX ENDOF
   ;MATCH ;
: GC-EMIT-SAXPY ( -- n )  GC-PRELUDE  s" tools/ptx/saxpy-cg.f" >LEN PROC-ARGV+  GC-RUN-EMIT ;
: GC-EMIT-RELU  ( -- n )  GC-PRELUDE  s" tools/ptx/relu-cg.f"  >LEN PROC-ARGV+  GC-RUN-EMIT ;
: GC-EMIT-EXP   ( -- n )  GC-PRELUDE  s" tools/ptx/exp-cg.f"   >LEN PROC-ARGV+  GC-RUN-EMIT ;
: GC-EMIT-EXPBWD ( -- n ) GC-PRELUDE  s" tools/ptx/expbwd-cg.f" >LEN PROC-ARGV+  GC-RUN-EMIT ;

: GC-PTXAS ( -- n )
   ATGT:LABEL$ PTXTC:TC-ARCH!                        \ assembler arch from the probed active target
   GC-QOUT $1000 >LEN GC-QERR $1000 >LEN PTXTC:ASSEMBLE ;

\ ctx + DX/DY are long-lived across all four kernels; the run scope owns them so a throw
\ never leaves the primary context retained (which hangs bin/hb at exit) or DX/DY allocated.
: GC-CTX-INIT ( -- )
   CUDA:OPEN
   0 CUDA:CU-INIT CUDA:RC0
   GC-DEV 0 >IDX CUDA:CU-DEVICE-GET CUDA:RC0
   GC-CTX GC-DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RETAIN CUDA:RC0
   GC-DEV @ >CUDA-DEV CUDA-SCOPE:OWN-PRIMARY-CTX
   GC-CTX @ >CUDA-CTX CUDA:CU-CTX-SET-CURRENT CUDA:RC0
   GC-DX 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0  GC-DX @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   GC-DY 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0  GC-DY @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR ;
\ the module is a per-kernel mutable slot (loaded then unloaded for each of the four kernels);
\ its reload cannot be a single LIFO-owned row, so GC-LOAD/GC-UNLOAD keep managing it explicitly.
: GC-LOAD ( -- )
   PTXTC:CUBIN$ GC-PATH FFI:CSTR
   GC-MOD GC-PATH CUDA:CU-MODULE-LOAD CUDA:RC0
   s" SAXPY" GC-KN FFI:CSTR
   GC-FUNC GC-MOD @ >CUDA-MOD GC-KN CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0 ;
: GC-UNLOAD ( -- )  GC-MOD @ >CUDA-MOD CUDA:CU-MODULE-UNLOAD CUDA:RC0 ;

\ launch the loaded kernel at x = xbits (a=3.0, y=0, n=1) -> z[0] f32 bits
: GC-AT ( n -- n ) {: xbits :}
   GC-RBUF 4 PTXSENT:FILL                                            \ poison readback: dropped copy-back fails closed
   GC-DX @ >CUDA-DEVPTR xbits 1 >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0
   GC-DY @ >CUDA-DEVPTR 0 1 >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0
   $40400000 GC-AB !  1 GC-NV !
   GC-FUNC @ >CUDA-FN 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   GC-FUNC @ >CUDA-FN 24 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   GC-FUNC @ >CUDA-FN 0 >IDX  GC-DX 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   GC-FUNC @ >CUDA-FN 8 >IDX  GC-DY 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   GC-FUNC @ >CUDA-FN 16 >IDX GC-AB 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   GC-FUNC @ >CUDA-FN 20 >IDX GC-NV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   GC-FUNC @ >CUDA-FN 1 1 CUDA:CU-LAUNCH-GRID CUDA:RC0
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0
   GC-RBUF GC-DY @ >CUDA-DEVPTR 4 >LEN CUDA:DTOH
   GC-RBUF @ $FFFFFFFF and PTXSENT:GUARD ;

\ launch a 2-input backward kernel: x=dz, y=savedy (n=1) -> result in x[0] f32 bits
: GC-AT-2IN ( n n -- n ) {: dzbits sybits :}
   GC-RBUF 4 PTXSENT:FILL                                            \ poison readback: dropped copy-back fails closed
   GC-DX @ >CUDA-DEVPTR dzbits 1 >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0  \ dz
   GC-DY @ >CUDA-DEVPTR sybits 1 >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0  \ savedy
   $40400000 GC-AB !  1 GC-NV !
   GC-FUNC @ >CUDA-FN 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   GC-FUNC @ >CUDA-FN 24 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   GC-FUNC @ >CUDA-FN 0 >IDX  GC-DX 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   GC-FUNC @ >CUDA-FN 8 >IDX  GC-DY 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   GC-FUNC @ >CUDA-FN 16 >IDX GC-AB 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   GC-FUNC @ >CUDA-FN 20 >IDX GC-NV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   GC-FUNC @ >CUDA-FN 1 1 CUDA:CU-LAUNCH-GRID CUDA:RC0
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0
   GC-RBUF GC-DX @ >CUDA-DEVPTR 4 >LEN CUDA:DTOH                      \ backward output in dz (x)
   GC-RBUF @ $FFFFFFFF and PTXSENT:GUARD ;

\ central difference of the loaded kernel w.r.t. x at x0, step h -> a Habu float
: GC-CENTRAL ( r r -- r ) {: x0 h :}
   x0 h f+ F32:NARROW GC-AT F32:WIDEN {: zp:r :}
   x0 h f- F32:NARROW GC-AT F32:WIDEN {: zm:r :}
   zp zm f-  h 2.0 f* f/ ;

: GC-NEAR? ( r r -- bool ) {: a b :}  a b f- {: d :}  d 0.0 f< if 0.0 d f- else d then  0.05 f< ;

\ device body run under the scope: GC-CTX-INIT owns ctx + DX/DY; the module is reloaded per
\ kernel (GC-LOAD/GC-UNLOAD). Asserts run before the frame exits - a failed assert throws and
\ the frame still releases the context (no hang) and frees DX/DY.
: GC-BODY ( -- )
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
   1.0 F32:NARROW GC-AT F32:WIDEN {: ey:r :}                  \ exp(1) = the analytic gradient
   GC-UNLOAD
   \ --- EXP BACKWARD KERNEL (resolved SAVED-Y): dx = dz * savedy, run on device ---
   GC-EMIT-EXPBWD drop GC-PTXAS 0 T=  GC-LOAD
   $3F800000 ey F32:NARROW GC-AT-2IN F32:WIDEN {: gb:r :}     \ backward(dz=1.0, savedy=exp(1)) = exp(1)
   GC-UNLOAD
   gs 3.0 GC-NEAR? TTRUE                               \ SAXPY: correct dx=a=3 -> PASS
   gs 2.0 GC-NEAR? TFALSE                              \ SAXPY: wrong dx=2 -> REJECTED
   gp 1.0 GC-NEAR? TTRUE                               \ RELU x>0: dx=1 -> PASS
   gm 0.0 GC-NEAR? TTRUE                               \ RELU x<0: dx=0 -> PASS
   gm 1.0 GC-NEAR? TFALSE                              \ RELU x<0: wrong dx=1 -> REJECTED
   ge ey GC-NEAR? TTRUE                                \ EXP: d exp/dx = exp(x) -> PASS
   ge 1.0 GC-NEAR? TFALSE                              \ EXP: wrong constant dx=1 -> REJECTED
   gb ge GC-NEAR? TTRUE ;                              \ EXP BACKWARD KERNEL output = numeric gradient

: GRADCHECK-MAIN ( -- )
   T-RESET
   s" habu-ptx-gradcheck" PTXTC:PREPARE
   [: GC-BODY ;] CUDA-SCOPE:SCOPE                      \ run + unwind ctx/DX/DY on return or throw
   PTXTC:CLEAN
   s" device gradcheck: SAXPY/RELU/EXP forward gradients AND the resolved EXP backward KERNEL (dz*savedy) verified on the Orin" type cr
   T-REPORT ;

GRADCHECK-MAIN

;package
