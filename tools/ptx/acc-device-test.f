\ acc-device-test.f - committed device-correctness regression for the register-accumulator
\ codegen. Self-contained: spawns bin/hb to emit the CHECKED AXPY-ACC kernel (tools/ptx/
\ acc-cg.f, which lowers ACC-ZERO/ACC-FMA/ACC-TILE via lib/ptx/cg.f) to a private PTX,
\ ptxas-assembles, then launches on the Orin with x=2.0, y=3.0, n=4 and asserts the result
\ y[0] = x*y = 6.0 (FP32 exact). Proves the (c) accumulator path is not just type-checked
\ but device-correct. Orin-only (FFI device launch). Load after lib/test.f, lib/ffi.f, and
\ the fs/process libs.

require lib/ptx/toolchain.f
require lib/ptx/sentinel.f

create AD-LIB 16 allot  create AD-NM 64 allot  create AD-PATH 64 allot  create AD-KN 32 allot
variable AD-H variable AD-DEV variable AD-CTX variable AD-MOD variable AD-FUNC
variable AD-DX variable AD-DY variable AD-AB variable AD-NV variable AD-RBUF
create AD-OUT $4000 allot  create AD-ERR $1000 allot
create AD-QOUT $1000 allot create AD-QERR $1000 allot

: AD-SYM ( ptr u8 n -- n )  AD-NM >CSTR  AD-H @ AD-NM DLSYM ;

\ spawn bin/hb to emit AXPY-ACC; return captured bytes
: AD-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"     >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f"  >LEN PROC-ARGV+
   s" lib/ptx/header.f"     >LEN PROC-ARGV+  s" lib/ptx/tile.f" >LEN PROC-ARGV+
   s" lib/ptx/tile-acc.f"   >LEN PROC-ARGV+  s" tools/ptx/acc-cg.f" >LEN PROC-ARGV+
   s" bin/hb" >LEN  AD-OUT $4000 >LEN  AD-ERR $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}
   PTXTC:PTX$ AD-OUT outu LEN>N WRITE-ALL  outu LEN>N ;

: AD-PTXAS ( -- n )
   AD-QOUT $1000 >LEN AD-QERR $1000 >LEN PTXTC:ASSEMBLE ;

\ launch x=2.0 y=3.0 n=4 -> y[0] f32 bits
: AD-RUN ( -- n )
   AD-RBUF 4 PTXSENT:FILL                                            \ poison readback: dropped copy-back fails closed
   s" libcuda.so.1" AD-LIB >CSTR  AD-LIB RTLD-NOW DLOPEN AD-H !
   0                       s" cuInit"                    AD-SYM CALL1 drop
   AD-DEV P>N 0            s" cuDeviceGet"               AD-SYM CALL2 drop
   AD-CTX P>N AD-DEV @     s" cuDevicePrimaryCtxRetain"  AD-SYM CALL2 drop
   AD-CTX @               s" cuCtxSetCurrent"           AD-SYM CALL1 drop
   PTXTC:CUBIN$ AD-PATH >CSTR
   AD-MOD P>N AD-PATH P>N s" cuModuleLoad"              AD-SYM CALL2 drop
   s" SAXPY" AD-KN >CSTR
   AD-FUNC P>N AD-MOD @ AD-KN P>N s" cuModuleGetFunction" AD-SYM CALL3 drop
   AD-DX P>N 16           s" cuMemAlloc_v2"   AD-SYM CALL2 drop
   AD-DY P>N 16           s" cuMemAlloc_v2"   AD-SYM CALL2 drop
   AD-DX @ $40000000 4    s" cuMemsetD32_v2"  AD-SYM CALL3 drop      \ x = 2.0
   AD-DY @ $40400000 4    s" cuMemsetD32_v2"  AD-SYM CALL3 drop      \ y = 3.0
   0 AD-AB !  4 AD-NV !                                              \ a unused, n = 4
   AD-FUNC @ 256 1 1      s" cuFuncSetBlockShape" AD-SYM CALL4 drop
   AD-FUNC @ 24           s" cuParamSetSize"  AD-SYM CALL2 drop
   AD-FUNC @ 0  AD-DX P>N 8  s" cuParamSetv"  AD-SYM CALL4 drop
   AD-FUNC @ 8  AD-DY P>N 8  s" cuParamSetv"  AD-SYM CALL4 drop
   AD-FUNC @ 16 AD-AB P>N 4  s" cuParamSetv"  AD-SYM CALL4 drop
   AD-FUNC @ 20 AD-NV P>N 4  s" cuParamSetv"  AD-SYM CALL4 drop
   AD-FUNC @ 1 1          s" cuLaunchGrid"    AD-SYM CALL3 drop
   0                      s" cuCtxSynchronize" AD-SYM CALL1 drop
   AD-RBUF P>N AD-DY @ 4  s" cuMemcpyDtoH_v2" AD-SYM CALL3 drop
   AD-MOD @  s" cuModuleUnload"            AD-SYM CALL1 drop
   AD-DEV @  s" cuDevicePrimaryCtxRelease" AD-SYM CALL1 drop
   AD-RBUF @ $FFFFFFFF and PTXSENT:GUARD ;

: ACC-DEV-MAIN ( -- )
   T-RESET
   s" habu-ptx-acc" PTXTC:PREPARE
   AD-EMIT drop
   AD-PTXAS 0 T=
   AD-RUN $40C00000 T=                  \ x*y = 2.0 * 3.0 = 6.0 (FP32 exact)
   PTXTC:CLEAN
   s" device: checked AXPY-ACC (ACC-ZERO/ACC-FMA/ACC-TILE) computes x*y=6.0 on the Orin" type cr
   T-REPORT ;

ACC-DEV-MAIN
