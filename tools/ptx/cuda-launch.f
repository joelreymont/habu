\ cuda-launch.f - CHECKED on-device proof: LAUNCH a checked-emitted SAXPY kernel
\ on the Orin GPU and verify the result against the CPU golden.
\
\ Fully checked Habu via lib/ffi.f (NO 0 set-check; only P>N/N>P trusted). The
\ deprecated <=8-arg launch API (cuFuncSetBlockShape / cuParamSetv / cuLaunchGrid)
\ avoids cuLaunchKernel's 11 args; the real driver memory entry points are the
\ _v2 symbols (the earlier INVALID_CONTEXT was symbol versioning). Prereq: cubin
\ at /tmp/saxpy.cubin. Data: x=2.0, y=0, n=4, ARBITRARY a marshalled through
\ F64>F32 (lib/ptx/cg.f) => y' = a*x+y = 2a. The host-side marshalling assertions
\ run unconditionally; the device leg is SKIPPED off-Orin (libcuda absent).

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ffi.f
require lib/test.f

create LL-LIB  16 allot
create LL-NM   64 allot          \ symbol-name scratch
create LL-PATH 64 allot
create LL-KN   32 allot          \ kernel name (separate from LL-NM)
variable LL-H  variable LL-DEV  variable LL-CTX  variable LL-MOD  variable LL-FUNC
variable LL-DX variable LL-DY variable LL-ABITS variable LL-NV variable LL-RBUF

: LL-OPEN ( -- )  s" libcuda.so.1" LL-LIB >CSTR  LL-LIB RTLD-NOW DLOPEN LL-H ! ;
: LL-SYM ( ptr u8 n -- n )  LL-NM >CSTR  LL-H @ LL-NM DLSYM ;

: LL-SETUP ( -- )                                 \ ctx + module + function
   0                       s" cuInit"                   LL-SYM CALL1 drop
   LL-DEV P>N 0            s" cuDeviceGet"              LL-SYM CALL2 drop
   LL-CTX P>N LL-DEV @     s" cuDevicePrimaryCtxRetain" LL-SYM CALL2 drop
   LL-CTX @               s" cuCtxSetCurrent"          LL-SYM CALL1 drop
   s" /tmp/saxpy.cubin" LL-PATH >CSTR
   LL-MOD P>N LL-PATH P>N s" cuModuleLoad"             LL-SYM CALL2 drop
   s" SAXPY" LL-KN >CSTR
   LL-FUNC P>N LL-MOD @ LL-KN P>N s" cuModuleGetFunction" LL-SYM CALL3 drop ;

: LL-LAUNCH ( r -- )  {: a:r :}                   \ marshal a, alloc, launch, copy back, free
   LL-DX P>N 16           s" cuMemAlloc_v2"   LL-SYM CALL2 drop
   LL-DY P>N 16           s" cuMemAlloc_v2"   LL-SYM CALL2 drop
   LL-DX @ 2.0 F64>F32 4  s" cuMemsetD32_v2"  LL-SYM CALL3 drop   \ x = 2.0
   LL-DY @ 0 4            s" cuMemsetD32_v2"  LL-SYM CALL3 drop   \ y = 0
   a F64>F32 LL-ABITS !  4 LL-NV !                                \ arbitrary a, n = 4
   LL-FUNC @ 256 1 1      s" cuFuncSetBlockShape" LL-SYM CALL4 drop
   LL-FUNC @ 24           s" cuParamSetSize"  LL-SYM CALL2 drop
   LL-FUNC @ 0  LL-DX P>N 8    s" cuParamSetv" LL-SYM CALL4 drop  \ p_x
   LL-FUNC @ 8  LL-DY P>N 8    s" cuParamSetv" LL-SYM CALL4 drop  \ p_y
   LL-FUNC @ 16 LL-ABITS P>N 4 s" cuParamSetv" LL-SYM CALL4 drop  \ p_a
   LL-FUNC @ 20 LL-NV P>N 4    s" cuParamSetv" LL-SYM CALL4 drop  \ p_n
   LL-FUNC @ 1 1          s" cuLaunchGrid"    LL-SYM CALL3 drop
   0                      s" cuCtxSynchronize" LL-SYM CALL1 drop
   LL-RBUF P>N LL-DY @ 4  s" cuMemcpyDtoH_v2" LL-SYM CALL3 drop
   LL-DX @  s" cuMemFree_v2" LL-SYM CALL1 drop
   LL-DY @  s" cuMemFree_v2" LL-SYM CALL1 drop ;

: LL-RELEASE ( -- )
   LL-MOD @  s" cuModuleUnload"          LL-SYM CALL1 drop
   LL-DEV @  s" cuDevicePrimaryCtxRelease" LL-SYM CALL1 drop ;

: SAXPY-GPU-BITS ( -- n )  LL-RBUF @ $FFFFFFFF and ;   \ read-back f32 bits

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
   LL-OPEN
   LL-H @ 0= if
      s" cuda-launch: libcuda unavailable -> device leg SKIPPED (host marshalling proven)" type cr
      T-REPORT exit
   then
   LL-SETUP
   3.0 LL-LAUNCH  3.0 SAXPY-CHECK       \ regression: a=3.0 golden 0x40C00000
   1.7 LL-LAUNCH  1.7 SAXPY-CHECK       \ arbitrary: a=1.7 golden 0x4059999A
   LL-RELEASE
   T-REPORT ;

RUN
