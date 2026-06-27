\ matmul-device-test.f - committed device-correctness regression for the tiled SGEMM
\ (lib/ptx/cg-matmul.f). Self-contained from the tree (retires the /tmp-only check the
\ goal flags): spawns bin/hb to emit MM -> /tmp/mm.ptx, ptxas-assembles, then launches
\ 64x64x64 with A=B=all-ones on the Orin and asserts C[0][0] = K = 64.0 (FP32 exact).
\ Orin-only (FFI device launch). Load after maki/eval.f + maki/eval-device.f (reuses
\ ED-SYM / CALL* / ED-* + the dlopen) and the fs/process libs. Closes (for the GEMM)
\ habu-committed-device-correctness.

create MMT-OUT $8000 allot  create MMT-ERR $1000 allot
create MQ-OUT  $1000 allot   create MQ-ERR  $1000 allot
variable MM-DA  variable MM-DB  variable MM-DC  variable MM-RB

\ spawn bin/hb to emit MM (cg-matmul.f) to /tmp/mm.ptx; return captured bytes
: MMT-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f" >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"    >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f" >LEN PROC-ARGV+
   s" lib/ptx/cg-matmul.f"  >LEN PROC-ARGV+  s" tools/ptx/matmul-cg.f" >LEN PROC-ARGV+
   s" bin/hb" >LEN  MMT-OUT $8000 >LEN  MMT-ERR $1000 >LEN  30000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}
   s" /tmp/mm.ptx" MMT-OUT outu LEN>N WRITE-ALL  outu LEN>N ;

\ assemble /tmp/mm.ptx -> /tmp/mm.cubin; return rc
: MMT-PTXAS ( -- n )
   PROC-ARGV-RESET
   s" -arch=sm_87"  >LEN PROC-ARGV+  s" /tmp/mm.ptx" >LEN PROC-ARGV+
   s" -o"           >LEN PROC-ARGV+  s" /tmp/mm.cubin" >LEN PROC-ARGV+
   s" /usr/local/cuda-12.6/bin/ptxas" >LEN  MQ-OUT $1000 >LEN  MQ-ERR $1000 >LEN  10000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}  rc RC>N ;

\ launch MM 64x64x64 with A=B=1.0 (one 64x64 block); return C[0][0] f32 bits
: MM-DEV ( -- n )
   s" libcuda.so.1" ED-LIB >CSTR  ED-LIB RTLD-NOW DLOPEN ED-H !
   0                       s" cuInit"                    ED-SYM CALL1 drop
   ED-DEV P>N 0            s" cuDeviceGet"               ED-SYM CALL2 drop
   ED-CTX P>N ED-DEV @     s" cuDevicePrimaryCtxRetain"  ED-SYM CALL2 drop
   ED-CTX @               s" cuCtxSetCurrent"           ED-SYM CALL1 drop
   s" /tmp/mm.cubin" ED-PATH >CSTR
   ED-MOD P>N ED-PATH P>N s" cuModuleLoad"              ED-SYM CALL2 drop
   s" MM" ED-KN >CSTR
   ED-FUNC P>N ED-MOD @ ED-KN P>N s" cuModuleGetFunction" ED-SYM CALL3 drop
   MM-DA P>N 16384        s" cuMemAlloc_v2"   ED-SYM CALL2 drop
   MM-DB P>N 16384        s" cuMemAlloc_v2"   ED-SYM CALL2 drop
   MM-DC P>N 16384        s" cuMemAlloc_v2"   ED-SYM CALL2 drop
   MM-DA @ $3F800000 4096 s" cuMemsetD32_v2"  ED-SYM CALL3 drop      \ A = 1.0
   MM-DB @ $3F800000 4096 s" cuMemsetD32_v2"  ED-SYM CALL3 drop      \ B = 1.0
   MM-DC @ 0 4096         s" cuMemsetD32_v2"  ED-SYM CALL3 drop      \ C = 0
   64 ED-NV !                                                        \ M=N=K=64 (reuse ED-NV)
   ED-FUNC @ 16 16 1      s" cuFuncSetBlockShape" ED-SYM CALL4 drop
   ED-FUNC @ 36           s" cuParamSetSize"  ED-SYM CALL2 drop
   ED-FUNC @ 0  MM-DA P>N 8  s" cuParamSetv"  ED-SYM CALL4 drop
   ED-FUNC @ 8  MM-DB P>N 8  s" cuParamSetv"  ED-SYM CALL4 drop
   ED-FUNC @ 16 MM-DC P>N 8  s" cuParamSetv"  ED-SYM CALL4 drop
   ED-FUNC @ 24 ED-NV P>N 4  s" cuParamSetv"  ED-SYM CALL4 drop      \ M
   ED-FUNC @ 28 ED-NV P>N 4  s" cuParamSetv"  ED-SYM CALL4 drop      \ N
   ED-FUNC @ 32 ED-NV P>N 4  s" cuParamSetv"  ED-SYM CALL4 drop      \ K
   ED-FUNC @ 1 1          s" cuLaunchGrid"    ED-SYM CALL3 drop
   0                      s" cuCtxSynchronize" ED-SYM CALL1 drop
   MM-RB P>N MM-DC @ 4    s" cuMemcpyDtoH_v2" ED-SYM CALL3 drop
   ED-MOD @  s" cuModuleUnload"            ED-SYM CALL1 drop
   ED-DEV @  s" cuDevicePrimaryCtxRelease" ED-SYM CALL1 drop
   MM-RB @ $FFFFFFFF and ;

T-RESET
MMT-EMIT 0 >  TTRUE                 \ emit produced PTX
MMT-PTXAS 0 = TTRUE                 \ ptxas assembled
MM-DEV $42800000 T=                 \ C[0][0] = K = 64.0 (FP32 exact)
s" matmul device test: 64^3 A=B=ones -> C=64.0 OK" type cr
T-REPORT
bye
