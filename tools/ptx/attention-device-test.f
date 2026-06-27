\ attention-device-test.f - committed device-correctness regression for the fused
\ attention kernel (lib/ptx/cg-attention.f). Self-contained from the tree: spawns
\ bin/hb to emit ATTN, ptxas-assembles, launches N=128 D=64 on the Orin with a
\ degenerate golden - Q=0 makes all scores 0 -> softmax uniform (1/N) -> O = mean_i V[i];
\ with V=all-ones that is O=1.0 exactly. Asserts O[0][0]=1.0. Orin-only (FFI). Load
\ after maki/eval.f + maki/eval-device.f + the fs/process libs. Advances
\ habu-committed-device-correctness (attention arm).

create ATT-OUT $8000 allot  create ATT-ERR $1000 allot
create AQ-OUT  $1000 allot   create AQ-ERR  $1000 allot
variable AT-DQ  variable AT-DK  variable AT-DV  variable AT-DO  variable AT-RB
variable AT-ND

: ATT-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f" >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"    >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f" >LEN PROC-ARGV+
   s" lib/ptx/cg-attention.f" >LEN PROC-ARGV+  s" tools/ptx/attention-cg.f" >LEN PROC-ARGV+
   s" bin/hb" >LEN  ATT-OUT $8000 >LEN  ATT-ERR $1000 >LEN  30000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}
   s" /tmp/attn.ptx" ATT-OUT outu LEN>N WRITE-ALL  outu LEN>N ;

: ATT-PTXAS ( -- n )
   PROC-ARGV-RESET
   s" -arch=sm_87"  >LEN PROC-ARGV+  s" /tmp/attn.ptx" >LEN PROC-ARGV+
   s" -o"           >LEN PROC-ARGV+  s" /tmp/attn.cubin" >LEN PROC-ARGV+
   s" /usr/local/cuda-12.6/bin/ptxas" >LEN  AQ-OUT $1000 >LEN  AQ-ERR $1000 >LEN  10000 >MS  RUN-ARGV-CAPTURE
   {: outu erru rc :}  rc RC>N ;

\ launch ATTN N=128 D=64 with Q=K=0, V=1.0; return O[0][0] f32 bits  (32768 = 128*64*4)
: ATT-DEV ( -- n )
   s" libcuda.so.1" ED-LIB >CSTR  ED-LIB RTLD-NOW DLOPEN ED-H !
   0                       s" cuInit"                    ED-SYM CALL1 drop
   ED-DEV P>N 0            s" cuDeviceGet"               ED-SYM CALL2 drop
   ED-CTX P>N ED-DEV @     s" cuDevicePrimaryCtxRetain"  ED-SYM CALL2 drop
   ED-CTX @               s" cuCtxSetCurrent"           ED-SYM CALL1 drop
   s" /tmp/attn.cubin" ED-PATH >CSTR
   ED-MOD P>N ED-PATH P>N s" cuModuleLoad"              ED-SYM CALL2 drop
   s" ATTN" ED-KN >CSTR
   ED-FUNC P>N ED-MOD @ ED-KN P>N s" cuModuleGetFunction" ED-SYM CALL3 drop
   AT-DQ P>N 32768        s" cuMemAlloc_v2"   ED-SYM CALL2 drop
   AT-DK P>N 32768        s" cuMemAlloc_v2"   ED-SYM CALL2 drop
   AT-DV P>N 32768        s" cuMemAlloc_v2"   ED-SYM CALL2 drop
   AT-DO P>N 32768        s" cuMemAlloc_v2"   ED-SYM CALL2 drop
   AT-DQ @ 0 8192         s" cuMemsetD32_v2"  ED-SYM CALL3 drop      \ Q = 0
   AT-DK @ 0 8192         s" cuMemsetD32_v2"  ED-SYM CALL3 drop      \ K = 0
   AT-DV @ $3F800000 8192 s" cuMemsetD32_v2"  ED-SYM CALL3 drop      \ V = 1.0
   AT-DO @ 0 8192         s" cuMemsetD32_v2"  ED-SYM CALL3 drop
   ED-FUNC @ 128 1 1      s" cuFuncSetBlockShape" ED-SYM CALL4 drop
   ED-FUNC @ 40           s" cuParamSetSize"  ED-SYM CALL2 drop
   ED-FUNC @ 0  AT-DQ P>N 8  s" cuParamSetv"  ED-SYM CALL4 drop
   ED-FUNC @ 8  AT-DK P>N 8  s" cuParamSetv"  ED-SYM CALL4 drop
   ED-FUNC @ 16 AT-DV P>N 8  s" cuParamSetv"  ED-SYM CALL4 drop
   ED-FUNC @ 24 AT-DO P>N 8  s" cuParamSetv"  ED-SYM CALL4 drop
   128 AT-ND !  ED-FUNC @ 32 AT-ND P>N 4 s" cuParamSetv" ED-SYM CALL4 drop   \ N=128
   64  AT-ND !  ED-FUNC @ 36 AT-ND P>N 4 s" cuParamSetv" ED-SYM CALL4 drop   \ D=64
   ED-FUNC @ 128 1        s" cuLaunchGrid"    ED-SYM CALL3 drop
   0                      s" cuCtxSynchronize" ED-SYM CALL1 drop
   AT-RB P>N AT-DO @ 4    s" cuMemcpyDtoH_v2" ED-SYM CALL3 drop
   ED-MOD @  s" cuModuleUnload"            ED-SYM CALL1 drop
   ED-DEV @  s" cuDevicePrimaryCtxRelease" ED-SYM CALL1 drop
   AT-RB @ $FFFFFFFF and ;

T-RESET
ATT-EMIT 0 >  TTRUE
ATT-PTXAS 0 = TTRUE
ATT-DEV $3F800000 T=                \ O[0][0] = mean(V) = 1.0
s" attention device test: Q=0,V=1 -> O=mean(V)=1.0 OK" type cr
T-REPORT
bye
