\ cuda-launch.f - CHECKED on-device proof: LAUNCH a checked-emitted scalar SAXPY
\ kernel on the Orin GPU and verify the result against the CPU golden.
\
\ Fully checked Habu via lib/ffi.f (NO 0 set-check; only P>N trusted). The
\ deprecated <=8-arg launch API (cuFuncSetBlockShape / cuParamSetv / cuLaunchGrid)
\ avoids cuLaunchKernel's 11 args; the real driver memory entry points are the
\ _v2 symbols (the earlier INVALID_CONTEXT was symbol versioning). Self-contained:
\ the cubin is SELF-EMITTED - a spawned bin/hb emits the checked SAXPY producer
\ (tools/ptx/saxpy-cg.f) to a PRIVATE per-run PTXTC PTX, ptxas-assembles it, and
\ this launches that private cubin. A missing producer or a nonzero emit/ptxas rc
\ fails CLOSED with the named E-PTX-EMIT throw (child stderr surfaced), never a
\ stale/absent /tmp/saxpy.cubin. A dropped copy-back fails closed via PTXSENT.
\ Data: x=2.0, y=0, n=4, ARBITRARY a marshalled through F64>F32 (lib/ptx/cg.f)
\ => y' = a*x+y = 2a. The host-side marshalling assertions run unconditionally;
\ the device leg (emit+ptxas+launch) is SKIPPED off-Orin (libcuda absent).

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ffi.f
require lib/ptx/toolchain.f
require lib/ptx/sentinel.f
require lib/ptx/cuda-driver.f
require lib/test.f
require lib/engine-candidate.f

create LL-PATH 64 allot
create LL-KN   32 allot          \ kernel name
create LL-OUT $8000 allot  create LL-ERR $1000 allot   \ spawned emit capture
create LL-QO  $1000 allot  create LL-QE  $1000 allot   \ ptxas capture
variable LL-DEV  variable LL-CTX  variable LL-MOD  variable LL-FUNC
variable LL-DX variable LL-DY variable LL-ABITS variable LL-NV variable LL-RBUF

\ spawn bin/hb to emit the checked scalar SAXPY kernel to the private PTX
: LL-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"     >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f"  >LEN PROC-ARGV+
   s" lib/ptx/header.f"     >LEN PROC-ARGV+  s" lib/ptx/tile.f" >LEN PROC-ARGV+
   s" tools/ptx/saxpy-cg.f" >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  LL-OUT $8000 >LEN  LL-ERR $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   LL-ERR erru LEN>N  rc RC>N  PTXTC:EMIT-GUARD           \ missing producer / nonzero emit rc -> stderr + throw
   PTXTC:PTX$ LL-OUT outu LEN>N WRITE-ALL  outu LEN>N ;

: LL-PTXAS ( -- n )
   LL-QO $1000 >LEN LL-QE $1000 >LEN PTXTC:ASSEMBLE ;

: LL-SETUP ( -- )                                 \ ctx + module (private cubin) + function
   CUDA:OPEN
   0 CUDA:CU-INIT CUDA:RC0
   LL-DEV 0 >IDX CUDA:CU-DEVICE-GET CUDA:RC0
   LL-CTX LL-DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RETAIN CUDA:RC0
   LL-CTX @ >CUDA-CTX CUDA:CU-CTX-SET-CURRENT CUDA:RC0
   PTXTC:CUBIN$ LL-PATH >CSTR
   LL-MOD LL-PATH CUDA:CU-MODULE-LOAD CUDA:RC0
   s" SAXPY" LL-KN >CSTR
   LL-FUNC LL-MOD @ >CUDA-MOD LL-KN CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0 ;

: LL-LAUNCH ( r -- )  {: a:r :}                   \ marshal a, alloc, launch, copy back, free
   LL-RBUF 4 PTXSENT:FILL                                                  \ poison readback: dropped copy-back fails closed
   LL-DX 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   LL-DY 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   LL-DX @ >CUDA-DEVPTR 2.0 F64>F32 4 >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0   \ x = 2.0
   LL-DY @ >CUDA-DEVPTR 0 4 >COUNT CUDA:CU-MEMSET-D32 CUDA:RC0             \ y = 0
   a F64>F32 LL-ABITS !  4 LL-NV !                                        \ arbitrary a, n = 4
   LL-FUNC @ >CUDA-FN 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   LL-FUNC @ >CUDA-FN 24 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   LL-FUNC @ >CUDA-FN 0 >IDX  LL-DX 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0    \ p_x
   LL-FUNC @ >CUDA-FN 8 >IDX  LL-DY 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0    \ p_y
   LL-FUNC @ >CUDA-FN 16 >IDX LL-ABITS 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0 \ p_a
   LL-FUNC @ >CUDA-FN 20 >IDX LL-NV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0    \ p_n
   LL-FUNC @ >CUDA-FN 1 1 CUDA:CU-LAUNCH-GRID CUDA:RC0
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0
   LL-RBUF LL-DY @ >CUDA-DEVPTR 4 >LEN CUDA:DTOH
   LL-DX @ >CUDA-DEVPTR CUDA:CU-MEM-FREE CUDA:RC0
   LL-DY @ >CUDA-DEVPTR CUDA:CU-MEM-FREE CUDA:RC0 ;

: LL-RELEASE ( -- )
   LL-MOD @ >CUDA-MOD CUDA:CU-MODULE-UNLOAD CUDA:RC0
   LL-DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RELEASE CUDA:RC0 ;

: SAXPY-GPU-BITS ( -- n )  LL-RBUF @ $FFFFFFFF and PTXSENT:GUARD ;   \ read-back f32 bits (fail-closed on dropped copy-back)

\ host-side marshalling proof (runs off-device too): 3.0 still narrows to the old
\ hardcoded 0x40400000 (no behaviour change), 1.7 is an arbitrary scalar, and the
\ two CPU goldens (a*x = 2a for a in {3.0, 1.7}) are the f32 bits the device must
\ return.
: HOST-CHECK ( -- )
   3.0 F64>F32 $40400000 T=            \ a=3.0 marshals to the previously hardcoded bits
   1.7 F64>F32 $3FD9999A T=            \ a=1.7 arbitrary scalar marshals correctly
   3.0 2.0 f* F64>F32 $40C00000 T=     \ a=3.0 CPU golden 6.0
   1.7 2.0 f* F64>F32 $4059999A T= ;   \ a=1.7 CPU golden 3.4

: SAXPY-CHECK ( r -- )  {: a:r :}       \ device result == CPU golden f32(a*x+y), x=2 y=0
   SAXPY-GPU-BITS  a 2.0 f* F64>F32  T=
   s" SAXPY a*2 on GPU -> f32 bits " type SAXPY-GPU-BITS . cr ;

: RUN ( -- )
   T-RESET
   HOST-CHECK
   CUDA:OPEN? 0= if
      s" cuda-launch: libcuda unavailable -> device leg SKIPPED (host marshalling proven)" type cr
      T-REPORT exit
   then
   s" habu-ptx-cuda-launch" PTXTC:PREPARE
   LL-EMIT drop                         \ self-emit the checked scalar SAXPY PTX (fail-closed)
   LL-PTXAS PTXTC:ASM-REPORT 0 T=       \ ptxas rc 0 (stderr surfaced on failure)
   LL-SETUP
   3.0 LL-LAUNCH  3.0 SAXPY-CHECK       \ regression: a=3.0 golden 0x40C00000
   1.7 LL-LAUNCH  1.7 SAXPY-CHECK       \ arbitrary: a=1.7 golden 0x4059999A
   LL-RELEASE
   PTXTC:CLEAN
   T-REPORT ;

RUN
