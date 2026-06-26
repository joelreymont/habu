\ cuda-launch.f - on-device proof: LAUNCH a checked-emitted SAXPY kernel on the
\ Orin GPU and verify the result against the CPU golden, via the Habu FFI.
\
\ This closes the M3 device-run + M1d launch (the deprecated <=8-arg launch API:
\ cuFuncSetBlockShape / cuParamSetv / cuLaunchGrid - so cuLaunchKernel's 11 args
\ are not needed). KEY: the real CUDA driver entry points are the _v2 symbols
\ (cuMemAlloc_v2 / cuMemsetD32_v2 / cuMemcpyDtoH_v2); the un-versioned names are
\ deprecated stubs that return INVALID_CONTEXT. Uses cuDevicePrimaryCtxRetain (the
\ Orin's camera pipeline owns the primary context). FFI is an unchecked boundary.
\
\ Prereq: emit + assemble the cubin first:
\   bin/hb --load lib/errors.f lib/string.f src/arch/ptx/emit.f tools/ptx/saxpy.f > /tmp/saxpy.ptx
\   /usr/local/cuda-12.6/bin/ptxas -arch=sm_87 /tmp/saxpy.ptx -o /tmp/saxpy.cubin
\ Run:
\   bin/hb --load lib/errors.f lib/string.f tools/ptx/cuda-launch.f
\ Test data: x=2.0, y=0, a=3.0, n=4  =>  y' = a*x + y = 6.0 (f32 0x40C00000).

0 set-check

create LLIB 32 allot
create LSYM 64 allot
create LKN 32 allot
create LPATH 64 allot
create LA 8 cells allot
variable LH variable LDEV variable LCTX variable LMOD variable LFUNC
variable LDX variable LDY variable LABITS variable LNV variable LRBUF

: LCSTR! ( ptr u8 n dst -- ) {: src len dst :} len 0 ?do src i + c@ dst i + c! loop 0 dst len + c! ;
: LOPEN s" libcuda.so.1" LLIB LCSTR! LLIB LA ! 2 LA 8 + ! LA DLOPEN-SLOT @ ffi-call LH ! ;
: LSY ( ptr u8 n -- fn ) LSYM LCSTR! LH @ LA ! LSYM LA 8 + ! LA DLSYM-SLOT @ ffi-call ;
: L1 ( n fn -- n ) {: a fn :} a LA ! LA fn ffi-call ;
: L2 ( n n fn -- n ) {: a b fn :} a LA ! b LA 8 + ! LA fn ffi-call ;
: L3 ( n n n fn -- n ) {: a b c fn :} a LA ! b LA 8 + ! c LA 16 + ! LA fn ffi-call ;
: L4 ( n n n n fn -- n ) {: a b c d fn :} a LA ! b LA 8 + ! c LA 16 + ! d LA 24 + ! LA fn ffi-call ;

\ EXPECT-6.0 ( -- ) : print PASS/FAIL of the read-back f32 bits vs 0x40C00000
: LAUNCH-SAXPY ( -- )
   LOPEN
   0 s" cuInit" LSY L1 drop
   LDEV 0 s" cuDeviceGet" LSY L2 drop
   LCTX LDEV @ s" cuDevicePrimaryCtxRetain" LSY L2 drop
   LCTX @ s" cuCtxSetCurrent" LSY L1 drop
   s" /tmp/saxpy.cubin" LPATH LCSTR!
   LMOD LPATH s" cuModuleLoad" LSY L2 drop
   s" SAXPY" LKN LCSTR!
   LFUNC LMOD @ LKN s" cuModuleGetFunction" LSY L3 drop
   LDX 16 s" cuMemAlloc_v2" LSY L2 drop
   LDY 16 s" cuMemAlloc_v2" LSY L2 drop
   LDX @ $40000000 4 s" cuMemsetD32_v2" LSY L3 drop   \ x = 2.0
   LDY @ 0          4 s" cuMemsetD32_v2" LSY L3 drop   \ y = 0
   $40400000 LABITS !   4 LNV !                        \ a = 3.0, n = 4
   LFUNC @ 256 1 1 s" cuFuncSetBlockShape" LSY L4 drop
   LFUNC @ 24 s" cuParamSetSize" LSY L2 drop
   LFUNC @ 0  LDX    8 s" cuParamSetv" LSY L4 drop
   LFUNC @ 8  LDY    8 s" cuParamSetv" LSY L4 drop
   LFUNC @ 16 LABITS 4 s" cuParamSetv" LSY L4 drop
   LFUNC @ 20 LNV    4 s" cuParamSetv" LSY L4 drop
   LFUNC @ 1 1 s" cuLaunchGrid" LSY L3 drop
   0 s" cuCtxSynchronize" LSY L1 drop
   LRBUF LDY @ 4 s" cuMemcpyDtoH_v2" LSY L3 drop
   s" SAXPY on GPU: y = a*x+y = 3*2+0 -> f32 bits " type LRBUF @ $FFFFFFFF and . cr
   s" expected 0x40C00000 ; PASS? " type
   LRBUF @ $FFFFFFFF and $40C00000 = if s" yes" else s" NO" then type cr
   LMOD @ s" cuModuleUnload" LSY L1 drop
   LDEV @ s" cuDevicePrimaryCtxRelease" LSY L1 drop ;

LAUNCH-SAXPY
bye
