\ cuda-launch.f - CHECKED on-device proof: LAUNCH a checked-emitted SAXPY kernel
\ on the Orin GPU and verify the result against the CPU golden.
\
\ Fully checked Habu via lib/ffi.f (NO 0 set-check; only P>N/N>P trusted). The
\ deprecated <=8-arg launch API (cuFuncSetBlockShape / cuParamSetv / cuLaunchGrid)
\ avoids cuLaunchKernel's 11 args; the real driver memory entry points are the
\ _v2 symbols (the earlier INVALID_CONTEXT was symbol versioning). Load after
\ lib/errors.f, lib/string.f, lib/ffi.f. Prereq: cubin at /tmp/saxpy.cubin.
\ Data: x=2.0, y=0, a=3.0, n=4  =>  y' = a*x+y = 6.0 (f32 0x40C00000).

create LL-LIB  16 allot
create LL-NM   64 allot          \ symbol-name scratch
create LL-PATH 64 allot
create LL-KN   32 allot          \ kernel name (separate from LL-NM)
variable LL-H  variable LL-DEV  variable LL-CTX  variable LL-MOD  variable LL-FUNC
variable LL-DX variable LL-DY variable LL-ABITS variable LL-NV variable LL-RBUF

: LL-OPEN ( -- )  s" libcuda.so.1" LL-LIB >CSTR  LL-LIB RTLD-NOW DLOPEN LL-H ! ;
: LL-SYM ( ptr u8 n -- n )  LL-NM >CSTR  LL-H @ LL-NM DLSYM ;

: LL-SETUP ( -- )                                 \ ctx + module + function
   LL-OPEN
   0                       s" cuInit"                   LL-SYM CALL1 drop
   LL-DEV P>N 0            s" cuDeviceGet"              LL-SYM CALL2 drop
   LL-CTX P>N LL-DEV @     s" cuDevicePrimaryCtxRetain" LL-SYM CALL2 drop
   LL-CTX @               s" cuCtxSetCurrent"          LL-SYM CALL1 drop
   s" /tmp/saxpy.cubin" LL-PATH >CSTR
   LL-MOD P>N LL-PATH P>N s" cuModuleLoad"             LL-SYM CALL2 drop
   s" SAXPY" LL-KN >CSTR
   LL-FUNC P>N LL-MOD @ LL-KN P>N s" cuModuleGetFunction" LL-SYM CALL3 drop ;

: LL-LAUNCH ( -- )                                \ alloc, set params, launch, sync, copy back
   LL-DX P>N 16           s" cuMemAlloc_v2"   LL-SYM CALL2 drop
   LL-DY P>N 16           s" cuMemAlloc_v2"   LL-SYM CALL2 drop
   LL-DX @ $40000000 4    s" cuMemsetD32_v2"  LL-SYM CALL3 drop   \ x = 2.0
   LL-DY @ 0 4            s" cuMemsetD32_v2"  LL-SYM CALL3 drop   \ y = 0
   $40400000 LL-ABITS !  4 LL-NV !                                \ a = 3.0, n = 4
   LL-FUNC @ 256 1 1      s" cuFuncSetBlockShape" LL-SYM CALL4 drop
   LL-FUNC @ 24           s" cuParamSetSize"  LL-SYM CALL2 drop
   LL-FUNC @ 0  LL-DX P>N 8    s" cuParamSetv" LL-SYM CALL4 drop  \ p_x
   LL-FUNC @ 8  LL-DY P>N 8    s" cuParamSetv" LL-SYM CALL4 drop  \ p_y
   LL-FUNC @ 16 LL-ABITS P>N 4 s" cuParamSetv" LL-SYM CALL4 drop  \ p_a
   LL-FUNC @ 20 LL-NV P>N 4    s" cuParamSetv" LL-SYM CALL4 drop  \ p_n
   LL-FUNC @ 1 1          s" cuLaunchGrid"    LL-SYM CALL3 drop
   0                      s" cuCtxSynchronize" LL-SYM CALL1 drop
   LL-RBUF P>N LL-DY @ 4  s" cuMemcpyDtoH_v2" LL-SYM CALL3 drop ;

: LL-RELEASE ( -- )
   LL-MOD @  s" cuModuleUnload"          LL-SYM CALL1 drop
   LL-DEV @  s" cuDevicePrimaryCtxRelease" LL-SYM CALL1 drop ;

: SAXPY-GPU-BITS ( -- n )  LL-RBUF @ $FFFFFFFF and ;   \ read-back f32 bits

: LAUNCH-SAXPY ( -- )
   LL-SETUP LL-LAUNCH LL-RELEASE
   s" SAXPY on GPU: y=a*x+y=3*2+0 -> f32 bits " type SAXPY-GPU-BITS . cr
   s" expected 0x40C00000 ; PASS? " type
   SAXPY-GPU-BITS $40C00000 = if s" yes" else s" NO" then type cr ;

LAUNCH-SAXPY
bye
