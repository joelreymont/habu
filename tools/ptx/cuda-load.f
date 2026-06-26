\ cuda-load.f - on-device proof: load a checked-emitted SAXPY cubin as a live GPU
\ module on the Orin and obtain its function handle, via the Habu FFI (M1d, the
\ ≤8-arg subset). FFI + foreign calls are an unchecked boundary (0 set-check):
\ raw dlopen/dlsym slot reads + ffi-call. Run (cubin at /tmp/saxpy.cubin):
\   bin/hb --load lib/errors.f lib/string.f tools/ptx/cuda-load.f
\ Uses cuDevicePrimaryCtxRetain (NOT cuCtxCreate - the Orin's camera pipeline owns
\ the primary context; ptx.md Resolved-M1/M2 #7).

0 set-check

create CL-LIB 32 allot
create CL-SYM 64 allot
create CL-KNAME 32 allot
create CL-PATH 64 allot
create CL-ARGS 8 cells allot
variable CL-H  variable CL-DEV  variable CL-CTX  variable CL-MOD  variable CL-FUNC

: CL-CSTR! ( ptr u8 n dst -- ) {: src len dst :}
   len 0 ?do  src i + c@  dst i + c!  loop  0 dst len + c! ;
: CL-OPEN ( -- )
   s" libcuda.so.1" CL-LIB CL-CSTR!  CL-LIB CL-ARGS !  2 CL-ARGS 8 + !
   CL-ARGS DLOPEN-SLOT @ ffi-call CL-H ! ;
: CL-SYM ( ptr u8 n -- fn )
   CL-SYM CL-CSTR!  CL-H @ CL-ARGS !  CL-SYM CL-ARGS 8 + !
   CL-ARGS DLSYM-SLOT @ ffi-call ;
: CL1 ( n fn -- n ) {: a fn :} a CL-ARGS ! CL-ARGS fn ffi-call ;
: CL2 ( n n fn -- n ) {: a b fn :} a CL-ARGS ! b CL-ARGS 8 + ! CL-ARGS fn ffi-call ;
: CL3 ( n n n fn -- n ) {: a b c fn :} a CL-ARGS ! b CL-ARGS 8 + ! c CL-ARGS 16 + ! CL-ARGS fn ffi-call ;

: CL-CHECK ( n ptr u8 n -- ) type s" =" type . cr ;   \ ( rc label -- )

: CUDA-LOAD-SAXPY ( -- )
   CL-OPEN
   0 s" cuInit" CL-SYM CL1                                  s" cuInit" CL-CHECK
   CL-DEV 0 s" cuDeviceGet" CL-SYM CL2                      s" cuDeviceGet" CL-CHECK
   CL-CTX CL-DEV @ s" cuDevicePrimaryCtxRetain" CL-SYM CL2  s" cuPrimaryCtxRetain" CL-CHECK
   CL-CTX @ s" cuCtxSetCurrent" CL-SYM CL1                  s" cuCtxSetCurrent" CL-CHECK
   s" /tmp/saxpy.cubin" CL-PATH CL-CSTR!
   CL-MOD CL-PATH s" cuModuleLoad" CL-SYM CL2               s" cuModuleLoad" CL-CHECK
   s" SAXPY" CL-KNAME CL-CSTR!
   CL-FUNC CL-MOD @ CL-KNAME s" cuModuleGetFunction" CL-SYM CL3  s" cuModuleGetFunction" CL-CHECK
   s" SAXPY-loaded-on-GPU(func!=0)=" type CL-FUNC @ 0 <> . cr
   CL-MOD @ s" cuModuleUnload" CL-SYM CL1 drop
   CL-DEV @ s" cuDevicePrimaryCtxRelease" CL-SYM CL1 drop ;

CUDA-LOAD-SAXPY
bye
