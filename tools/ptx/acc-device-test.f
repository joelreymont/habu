\ acc-device-test.f - committed device-correctness regression for the register-accumulator
\ codegen. Self-contained: spawns bin/hb to emit the CHECKED AXPY-ACC kernel (tools/ptx/
\ acc-cg.f, which lowers ACC-ZERO/ACC-FMA/ACC-TILE via lib/ptx/cg.f) to a private PTX,
\ ptxas-assembles, then launches on the Orin with x=2.0, y=3.0, n=4 and asserts the result
\ y[0] = x*y = 6.0 (FP32 exact). Proves the (c) accumulator path is not just type-checked
\ but device-correct. Orin-only (FFI device launch). Load after lib/test.f, lib/ffi.f, and
\ the fs/process libs.

require lib/ptx/toolchain.f
require lib/ptx/sentinel.f
require lib/ptx/cuda-driver.f

create AD-PATH 64 allot  create AD-KN 32 allot
variable AD-DEV variable AD-CTX variable AD-MOD variable AD-FUNC
variable AD-DX variable AD-DY variable AD-AB variable AD-NV variable AD-RBUF
create AD-OUT $4000 allot  create AD-ERR $1000 allot
create AD-QOUT $1000 allot create AD-QERR $1000 allot

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
   CUDA:OPEN
   0 CUDA:CU-INIT CUDA:RC0
   AD-DEV 0 >IDX CUDA:CU-DEVICE-GET CUDA:RC0
   AD-CTX AD-DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RETAIN CUDA:RC0
   AD-CTX @ >CUDA-CTX CUDA:CU-CTX-SET-CURRENT CUDA:RC0
   PTXTC:CUBIN$ AD-PATH >CSTR
   AD-MOD AD-PATH CUDA:CU-MODULE-LOAD CUDA:RC0
   s" SAXPY" AD-KN >CSTR
   AD-FUNC AD-MOD @ >CUDA-MOD AD-KN CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0
   AD-DX 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   AD-DY 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   AD-DX @ >CUDA-DEVPTR $40000000 4 >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0   \ x = 2.0
   AD-DY @ >CUDA-DEVPTR $40400000 4 >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0   \ y = 3.0
   0 AD-AB !  4 AD-NV !                                              \ a unused, n = 4
   AD-FUNC @ >CUDA-FN 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   AD-FUNC @ >CUDA-FN 24 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   AD-FUNC @ >CUDA-FN 0 >IDX  AD-DX 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   AD-FUNC @ >CUDA-FN 8 >IDX  AD-DY 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   AD-FUNC @ >CUDA-FN 16 >IDX AD-AB 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   AD-FUNC @ >CUDA-FN 20 >IDX AD-NV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   AD-FUNC @ >CUDA-FN 1 1 CUDA:CU-LAUNCH-GRID CUDA:RC0
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0
   AD-RBUF AD-DY @ >CUDA-DEVPTR 4 >LEN CUDA:DTOH
   AD-MOD @ >CUDA-MOD CUDA:CU-MODULE-UNLOAD CUDA:RC0
   AD-DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RELEASE CUDA:RC0
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
