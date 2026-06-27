\ gradcheck.f - device-run central-difference gradcheck (the AD "hard gate" v0).
\
\ The checker proves TYPES, not the DERIVATIVE: a wrong VJP type-checks and ships a
\ silently wrong gradient. This harness closes that gap NUMERICALLY on the Orin: launch a
\ forward kernel at x+h and x-h, form the central difference (f(x+h)-f(x-h))/2h, and
\ compare it to the analytic VJP. Demonstrated on SAXPY (z = a*x + y, a=3): d z/d x = a = 3.
\ A correct analytic gradient passes; a wrong one (2) is rejected - so the harness can gate
\ every VJP entry / generated backward (extension to the auto-derived softmax backward is
\ dotted habu-ad-thread-saved + habu-ad-softmax-rows). Self-contained, Orin-only (FFI).
\
\ NB: GC-FINI MUST run (cuModuleUnload + cuDevicePrimaryCtxRelease) - a retained primary
\ context that is not released hangs bin/hb at process exit on the Orin (RCA
\ habu-rca-device-gradcheck). Load after lib/test.f, lib/ffi.f, lib/ptx/cg.f
\ (F32>F64/F64>F32), and the fs/process libs.

create GC-LIB 16 allot  create GC-NM 64 allot  create GC-PATH 64 allot  create GC-KN 32 allot
variable GC-H variable GC-DEV variable GC-CTX variable GC-MOD variable GC-FUNC
variable GC-DX variable GC-DY variable GC-AB variable GC-NV variable GC-RBUF
create GC-OUT $4000 allot  create GC-ERR $1000 allot
create GC-QOUT $1000 allot create GC-QERR $1000 allot

: GC-SYM ( ptr u8 n -- n )  GC-NM >CSTR  GC-H @ GC-NM DLSYM ;

\ spawn bin/hb to emit SAXPY -> /tmp/gc.ptx
: GC-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"     >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f"  >LEN PROC-ARGV+
   s" lib/ptx/header.f"     >LEN PROC-ARGV+  s" lib/ptx/tile.f" >LEN PROC-ARGV+
   s" tools/ptx/saxpy-cg.f" >LEN PROC-ARGV+
   s" bin/hb" >LEN  GC-OUT $4000 >LEN  GC-ERR $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}
   s" /tmp/gc.ptx" GC-OUT outu LEN>N WRITE-ALL  outu LEN>N ;

: GC-PTXAS ( -- n )
   PROC-ARGV-RESET
   s" -arch=sm_87"  >LEN PROC-ARGV+  s" /tmp/gc.ptx" >LEN PROC-ARGV+
   s" -o"           >LEN PROC-ARGV+  s" /tmp/gc.cubin" >LEN PROC-ARGV+
   s" /usr/local/cuda-12.6/bin/ptxas" >LEN  GC-QOUT $1000 >LEN  GC-QERR $1000 >LEN  10000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}  rc RC>N ;

\ one-time device + module + buffer setup
: GC-INIT ( -- )
   s" libcuda.so.1" GC-LIB >CSTR  GC-LIB RTLD-NOW DLOPEN GC-H !
   0                       s" cuInit"                    GC-SYM CALL1 drop
   GC-DEV P>N 0            s" cuDeviceGet"               GC-SYM CALL2 drop
   GC-CTX P>N GC-DEV @     s" cuDevicePrimaryCtxRetain"  GC-SYM CALL2 drop
   GC-CTX @               s" cuCtxSetCurrent"           GC-SYM CALL1 drop
   s" /tmp/gc.cubin" GC-PATH >CSTR
   GC-MOD P>N GC-PATH P>N s" cuModuleLoad"              GC-SYM CALL2 drop
   s" SAXPY" GC-KN >CSTR
   GC-FUNC P>N GC-MOD @ GC-KN P>N s" cuModuleGetFunction" GC-SYM CALL3 drop
   GC-DX P>N 16           s" cuMemAlloc_v2"   GC-SYM CALL2 drop
   GC-DY P>N 16           s" cuMemAlloc_v2"   GC-SYM CALL2 drop ;

\ MUST run before exit: release the primary context or bin/hb hangs at teardown
: GC-FINI ( -- )
   GC-MOD @  s" cuModuleUnload"            GC-SYM CALL1 drop
   GC-DEV @  s" cuDevicePrimaryCtxRelease" GC-SYM CALL1 drop ;

\ launch SAXPY at x = xbits (a=3.0, y=0, n=1) -> z[0] f32 bits = a*x
: GC-AT ( n -- n ) {: xbits :}
   GC-DX @ xbits 1        s" cuMemsetD32_v2"  GC-SYM CALL3 drop      \ x = xbits
   GC-DY @ 0 1            s" cuMemsetD32_v2"  GC-SYM CALL3 drop      \ y = 0
   $40400000 GC-AB !  1 GC-NV !                                     \ a = 3.0, n = 1
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

\ central difference of SAXPY w.r.t. x at x0, step h -> a Habu float
: GC-CENTRAL ( r r -- r ) {: x0 h :}
   x0 h f+ F64>F32 GC-AT F32>F64 {: zp :}                          \ z(x+h)
   x0 h f- F64>F32 GC-AT F32>F64 {: zm :}                          \ z(x-h)
   zp zm f-  h 2.0 f* f/ ;                                         \ (zp - zm) / 2h

: GC-NEAR? ( r r -- bool ) {: a b :}  a b f- {: d :}  d 0.0 f< if 0.0 d f- else d then  0.05 f< ;

: GRADCHECK-MAIN ( -- )
   T-RESET
   GC-EMIT drop
   GC-PTXAS 0 T=
   GC-INIT
   2.0 0.001 GC-CENTRAL {: g :}                                    \ numeric d(a*x)/dx at x=2
   GC-FINI                                                         \ release context BEFORE exit
   g 3.0 GC-NEAR? TTRUE                                            \ correct analytic VJP dx = a = 3 -> PASS
   g 2.0 GC-NEAR? TFALSE                                           \ a WRONG analytic VJP (2.0) is REJECTED
   s" device gradcheck: d(a*x)/dx = a = 3.0 verified by central difference on the Orin (wrong VJP rejected)" type cr
   T-REPORT ;

GRADCHECK-MAIN
