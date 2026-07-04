\ matmul-device-test.f - committed device-correctness regression for the tiled SGEMM
\ (lib/ptx/cg-matmul.f). Self-contained from the tree (retires the /tmp-only check the
\ goal flags): spawns bin/hb to emit MM to a private PTX, ptxas-assembles, then launches
\ 64x64x64 with A=B=all-ones on the Orin and asserts C[0][0] = K = 64.0 (FP32 exact).
\ Orin-only (FFI device launch). Uses the checked lib/ptx/cuda-driver.f (CUDA:CU-*);
\ load after maki/eval.f + maki/eval-device.f (reuses the ED-* handle cells) and the
\ fs/process libs. Closes (for the GEMM) habu-committed-device-correctness.

require lib/ptx/toolchain.f
require lib/ptx/sentinel.f
require lib/ptx/cuda-driver.f

create MMT-OUT $8000 allot  create MMT-ERR $1000 allot
create MQ-OUT  $1000 allot   create MQ-ERR  $1000 allot
variable MM-DA  variable MM-DB  variable MM-DC  variable MM-RB

\ spawn bin/hb to emit MM (cg-matmul.f); return captured bytes
: MMT-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f" >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"    >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f" >LEN PROC-ARGV+
   s" lib/ptx/cg-matmul.f"  >LEN PROC-ARGV+  s" tools/ptx/matmul-cg.f" >LEN PROC-ARGV+
   s" bin/hb" >LEN  MMT-OUT $8000 >LEN  MMT-ERR $1000 >LEN  30000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}
   PTXTC:PTX$ MMT-OUT outu LEN>N WRITE-ALL  outu LEN>N ;

: MMT-PTXAS ( -- n )
   MQ-OUT $1000 >LEN MQ-ERR $1000 >LEN PTXTC:ASSEMBLE ;

\ launch MM 64x64x64 with A=B=1.0 (one 64x64 block); return C[0][0] f32 bits
: MM-DEV ( -- n )
   MM-RB 4 PTXSENT:FILL                                              \ poison readback: dropped copy-back fails closed
   CUDA:OPEN
   0 CUDA:CU-INIT CUDA:RC0
   ED-DEV 0 >IDX CUDA:CU-DEVICE-GET CUDA:RC0
   ED-CTX ED-DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RETAIN CUDA:RC0
   ED-CTX @ >CUDA-CTX CUDA:CU-CTX-SET-CURRENT CUDA:RC0
   PTXTC:CUBIN$ ED-PATH >CSTR
   ED-MOD ED-PATH CUDA:CU-MODULE-LOAD CUDA:RC0
   s" MM" ED-KN >CSTR
   ED-FUNC ED-MOD @ >CUDA-MOD ED-KN CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0
   MM-DA 16384 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   MM-DB 16384 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   MM-DC 16384 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   MM-DA @ >CUDA-DEVPTR $3F800000 4096 >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0   \ A = 1.0
   MM-DB @ >CUDA-DEVPTR $3F800000 4096 >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0   \ B = 1.0
   MM-DC @ >CUDA-DEVPTR 0 4096 >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0           \ C = 0
   64 ED-NV !                                                        \ M=N=K=64 (reuse ED-NV)
   ED-FUNC @ >CUDA-FN 16 16 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   ED-FUNC @ >CUDA-FN 36 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   ED-FUNC @ >CUDA-FN 0 >IDX  MM-DA 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   ED-FUNC @ >CUDA-FN 8 >IDX  MM-DB 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   ED-FUNC @ >CUDA-FN 16 >IDX MM-DC 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   ED-FUNC @ >CUDA-FN 24 >IDX ED-NV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0     \ M
   ED-FUNC @ >CUDA-FN 28 >IDX ED-NV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0     \ N
   ED-FUNC @ >CUDA-FN 32 >IDX ED-NV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0     \ K
   ED-FUNC @ >CUDA-FN 1 1 CUDA:CU-LAUNCH-GRID CUDA:RC0
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0
   MM-RB MM-DC @ >CUDA-DEVPTR 4 >LEN CUDA:DTOH
   ED-MOD @ >CUDA-MOD CUDA:CU-MODULE-UNLOAD CUDA:RC0
   ED-DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RELEASE CUDA:RC0
   MM-RB @ $FFFFFFFF and PTXSENT:GUARD ;

T-RESET
s" habu-ptx-mm" PTXTC:PREPARE
MMT-EMIT 0 >  TTRUE                 \ emit produced PTX
MMT-PTXAS 0 = TTRUE                 \ ptxas assembled
MM-DEV $42800000 T=                 \ C[0][0] = K = 64.0 (FP32 exact)
PTXTC:CLEAN
s" matmul device test: 64^3 A=B=ones -> C=64.0 OK" type cr
T-REPORT
bye
