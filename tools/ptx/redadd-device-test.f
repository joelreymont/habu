\ redadd-device-test.f - device verification of red.global.add.f32 on sm_87 (closes
\ habu-ptx-ad-verify): the atomic float add scatter-add needs for fan-in adjoints. Spawns
\ bin/hb to emit tools/ptx/redadd-cg.f to a private PTX, ptxas-assembles, then launches 256
\ threads each atomically adding 1.0 to out[0] and asserts out[0] = 256.0 (FP32 exact). Orin-only.
\ Releases the primary context before exit (or bin/hb hangs at teardown). Load after lib/test.f,
\ lib/ffi.f, and the fs/process libs.

require lib/ptx/toolchain.f
require lib/ptx/sentinel.f
require lib/ptx/cuda-driver.f

create RA-PATH 64 allot  create RA-KN 32 allot
variable RA-DEV variable RA-CTX variable RA-MOD variable RA-FUNC
variable RA-OUT variable RA-NV variable RA-RBUF
create RA-O $4000 allot  create RA-E $1000 allot  create RA-QO $1000 allot create RA-QE $1000 allot

: RA-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f" >LEN PROC-ARGV+  s" lib/string.f" >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f" >LEN PROC-ARGV+  s" tools/ptx/redadd-cg.f" >LEN PROC-ARGV+
   s" bin/hb" >LEN  RA-O $4000 >LEN  RA-E $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   {: ou:len eu:len rc:rc :}  PTXTC:PTX$ RA-O ou LEN>N WRITE-ALL  ou LEN>N ;

: RA-PTXAS ( -- n )
   RA-QO $1000 >LEN RA-QE $1000 >LEN PTXTC:ASSEMBLE ;

: RA-RUN ( -- n )   \ launch 256 threads, out=0 -> out[0] f32 bits (= 256.0)
   RA-RBUF 4 PTXSENT:FILL                                            \ poison readback: dropped copy-back fails closed
   CUDA:OPEN
   0 CUDA:CU-INIT CUDA:RC0
   RA-DEV 0 >IDX CUDA:CU-DEVICE-GET CUDA:RC0
   RA-CTX RA-DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RETAIN CUDA:RC0
   RA-CTX @ >CUDA-CTX CUDA:CU-CTX-SET-CURRENT CUDA:RC0
   PTXTC:CUBIN$ RA-PATH >CSTR
   RA-MOD RA-PATH CUDA:CU-MODULE-LOAD CUDA:RC0
   s" REDADD" RA-KN >CSTR
   RA-FUNC RA-MOD @ >CUDA-MOD RA-KN CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0
   RA-OUT 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   RA-OUT @ >CUDA-DEVPTR 0 1 >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0      \ out = 0
   256 RA-NV !
   RA-FUNC @ >CUDA-FN 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   RA-FUNC @ >CUDA-FN 12 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   RA-FUNC @ >CUDA-FN 0 >IDX  RA-OUT 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   RA-FUNC @ >CUDA-FN 8 >IDX  RA-NV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   RA-FUNC @ >CUDA-FN 1 1 CUDA:CU-LAUNCH-GRID CUDA:RC0
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0
   RA-RBUF RA-OUT @ >CUDA-DEVPTR 4 >LEN CUDA:DTOH
   RA-MOD @ >CUDA-MOD CUDA:CU-MODULE-UNLOAD CUDA:RC0
   RA-DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RELEASE CUDA:RC0    \ release or bin/hb hangs at exit
   RA-RBUF @ $FFFFFFFF and PTXSENT:GUARD ;

: REDADD-MAIN ( -- )
   T-RESET
   s" habu-ptx-redadd" PTXTC:PREPARE
   RA-EMIT drop
   RA-PTXAS 0 T=
   RA-RUN $43800000 T=                  \ 256 atomic adds of 1.0 = 256.0
   PTXTC:CLEAN
   s" device: red.global.add.f32 verified on sm_87 - 256 atomic adds = 256.0 (scatter-add primitive OK)" type cr
   T-REPORT ;

REDADD-MAIN
