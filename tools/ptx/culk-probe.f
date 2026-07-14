\ culk-probe.f - RCA probe: launch the checked SAXPY kernel on the Orin via the
\ modern cuLaunchKernel entry point (11 args, kernelParams void**), to root-cause
\ why that path hangs cuCtxSynchronize while the deprecated cuParamSetv/cuLaunchGrid
\ path (tools/ptx/cuda-launch.f) runs golden. Dot habu-rca-culaunchkernel-ee5babba.
\
\ Reusable + fully checked (lib/ffi.f; only P>N/N>P trusted). cuLaunchKernel is not
\ in the shared bindings, so it is called here through the general FFI-CALLN
\ trampoline with EVERY one of the 11 args explicitly set (incl. extra=NULL) so a
\ stale shared-argbuf slot cannot leak in. Data: x=2.0, y=0, a=3.0, n=4 => y'=6.0
\ (f32 0x40C00000). Prereq: cubin at /tmp/saxpy.cubin. Load after lib/errors.f,
\ lib/string.f, lib/ffi.f.
\
\ HABU_CULK_MODE (env) selects a deliberate defect so the RCA can isolate the
\ historical hang without editing code:
\   unset / "good" = correct 11-arg call, extra NULL   (the intended modern call)
\   "badextra"     = extra = kernelParams (non-NULL)    (driver rejects: rc=1, no run)
\   "badptr"       = kernelParams[0] points at a bogus device address (rc=0 launch,
\                    kernel does an illegal global access -> clean rc=700, no hang)
\   "badgrid"      = gridDimX = a huge value in a REGISTER arg slot (what an early
\                    FFI-CALLN mis-spill delivered) -> rc=0 launch, sync HANGS

require lib/errors.f
require lib/string.f
require lib/ffi.f

create CK-LIB  16 allot
create CK-NM   64 allot
create CK-PATH 64 allot
create CK-KN   32 allot
variable CK-H  variable CK-DEV  variable CK-CTX  variable CK-MOD  variable CK-FUNC
variable CK-DX variable CK-DY variable CK-ABITS variable CK-NV variable CK-RBUF

$40000000 constant CK-X-BITS        \ 2.0f
$40400000 constant CK-A-BITS        \ 3.0f
$40C00000 constant CK-GOLDEN        \ 6.0f = a*x+y
256 constant CK-BLOCK
4 constant CK-N

: CK-OPEN ( -- )  s" libcuda.so.1" CK-LIB >CSTR  CK-LIB RTLD-NOW DLOPEN CK-H ! ;
: CK-SYM ( ptr u8 n -- n )  CK-NM >CSTR  CK-H @ CK-NM DLSYM ;

: CK-SETUP ( -- )                                 \ ctx + module + function
   CK-OPEN
   0                       s" cuInit"                   CK-SYM CALL1 drop
   CK-DEV P>N 0            s" cuDeviceGet"              CK-SYM CALL2 drop
   CK-CTX P>N CK-DEV @     s" cuDevicePrimaryCtxRetain" CK-SYM CALL2 drop
   CK-CTX @               s" cuCtxSetCurrent"          CK-SYM CALL1 drop
   s" /tmp/saxpy.cubin" CK-PATH >CSTR
   CK-MOD P>N CK-PATH P>N s" cuModuleLoad"             CK-SYM CALL2 drop
   s" SAXPY" CK-KN >CSTR
   CK-FUNC P>N CK-MOD @ CK-KN P>N s" cuModuleGetFunction" CK-SYM CALL3 drop ;

: CK-MODE? ( ptr u8 n -- bool )  s" HABU_CULK_MODE" GETENV 2swap STR= ;

$40000000 constant CK-HUGE-GRID                  \ 2^30 blocks: a valid-but-enormous launch

: CK-EXTRA ( n -- n ) {: kparams:n :}             \ the 11th arg per HABU_CULK_MODE
   s" badextra" CK-MODE? if kparams else 0 then ;

: CK-GRIDX ( -- n )                               \ gridDimX: 1, or huge for badgrid
   s" badgrid" CK-MODE? if CK-HUGE-GRID else 1 then ;

\ cuLaunchKernel(f, gx,gy,gz, bx,by,bz, shmem, stream, kernelParams, extra) - 11
\ integer/pointer args through FFI-CALLN. ALL slots set, so no stale argbuf leak.
: CK-LAUNCHKERNEL ( n n n -- n ) {: f:n kparams:n launchfn:n :}
   f        0 FFI-ARG!
   CK-GRIDX 1 FFI-ARG!   1 2 FFI-ARG!   1 3 FFI-ARG!    \ grid Gx1x1
   CK-BLOCK 4 FFI-ARG!   1 5 FFI-ARG!   1 6 FFI-ARG!    \ block 256x1x1
   0        7 FFI-ARG!                                  \ sharedMemBytes
   0        8 FFI-ARG!                                  \ hStream = default
   kparams  9 FFI-ARG!                                  \ kernelParams void**
   kparams CK-EXTRA 10 FFI-ARG!                         \ extra: NULL (good) or kparams (badextra)
   11 launchfn FFI-CALLN ;

$DEADBEEF00 constant CK-BOGUS-DEVPTR             \ non-NULL invalid device address

: CK-MAYBE-BADPTR ( -- )                          \ corrupt the x device ptr for the badptr mode
   s" HABU_CULK_MODE" GETENV s" badptr" STR= if CK-BOGUS-DEVPTR CK-DX ! then ;

: CK-PARAMS ( -- )                                \ build kernelParams = [&dx,&dy,&abits,&nv]
   CK-MAYBE-BADPTR
   FFI-KPARAM-RESET
   CK-DX FFI-KPARAM+  CK-DY FFI-KPARAM+  CK-ABITS FFI-KPARAM+  CK-NV FFI-KPARAM+ ;

: CK-LAUNCH ( -- )                                \ alloc, params, launch, sync, copy back
   CK-DX P>N 16           s" cuMemAlloc_v2"   CK-SYM CALL2 drop
   CK-DY P>N 16           s" cuMemAlloc_v2"   CK-SYM CALL2 drop
   CK-DX @ CK-X-BITS 4    s" cuMemsetD32_v2"  CK-SYM CALL3 drop   \ x = 2.0
   CK-DY @ 0 4            s" cuMemsetD32_v2"  CK-SYM CALL3 drop   \ y = 0
   CK-A-BITS CK-ABITS !  CK-N CK-NV !                             \ a = 3.0, n = 4
   CK-PARAMS
   CK-FUNC @ FFI-KPARAMS>N s" cuLaunchKernel" CK-SYM CK-LAUNCHKERNEL
   s" cuLaunchKernel rc=" type . cr
   0                      s" cuCtxSynchronize" CK-SYM CALL1
   s" cuCtxSynchronize rc=" type . cr
   CK-RBUF P>N CK-DY @ 4  s" cuMemcpyDtoH_v2" CK-SYM CALL3 drop ;

: CK-RELEASE ( -- )
   CK-MOD @  s" cuModuleUnload"          CK-SYM CALL1 drop
   CK-DEV @  s" cuDevicePrimaryCtxRelease" CK-SYM CALL1 drop ;

: CK-GPU-BITS ( -- n )  CK-RBUF @ $FFFFFFFF and ;

: LAUNCH-CULK ( -- )
   CK-SETUP CK-LAUNCH CK-RELEASE
   s" SAXPY via cuLaunchKernel: f32 bits " type CK-GPU-BITS . cr
   s" expected 0x40C00000 ; PASS? " type
   CK-GPU-BITS CK-GOLDEN = if s" yes" else s" NO" then type cr ;

LAUNCH-CULK
bye
