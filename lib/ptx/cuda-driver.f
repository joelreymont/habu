\ cuda-driver.f - checked CUDA Driver API: nominal handle roles, FFI bindings,
\ fail-closed guards, and typed convenience helpers.
\
\ The canonical checked CUDA Driver lives here in the stdlib (package CUDA) so
\ every consumer - maki eval/lowering and the tools/ptx launchers - shares one
\ typed resolver instead of hand-rolling raw DLSYM + per-file rc/die plumbing.
\ Handle roles (cuda-dev/ctx/mod/fn/devptr) are nominal cell types: the checker
\ never collapses a device pointer into a module handle. Every driver call
\ returns a nominal rc that RC0 fail-closes on; every out-handle passes through
\ HANDLE0 so a null handle throws E-CUDA instead of masquerading as valid.
\
\ Word names are repo-standard hyphenated (CU-DEVICE-GET); the SYMBOL clause
\ carries the exact C entry point (cuDeviceGet). The deprecated <=8-arg launch
\ API (cuFuncSetBlockShape / cuParamSetv / cuLaunchGrid) avoids cuLaunchKernel's
\ 11 args; the real driver memory entry points are the _v2 symbols.

require lib/ffi.f

-5002 constant E-CUDA          \ CUDA Driver call failed: null handle or nonzero CUresult

\ Nominal handle roles - one cell each at runtime, distinct nominal types to the
\ checker. Defined at top level so the >CUDA-* / CUDA-*>N casts stay global.
deftype cuda-dev
deftype cuda-ctx
deftype cuda-mod
deftype cuda-fn
deftype cuda-devptr
deftype cuda-event

\ Fail-closed guards, global so historical maki call sites (CUDA-HANDLE0 /
\ CUDA-RC0) resolve unqualified.
: CUDA-HANDLE0 ( n -- n )
   dup 0= if E-CUDA throw then ;

: CUDA-RC0 ( rc -- )
   RC>N dup 0 <> if E-CUDA throw then
   drop ;

package CUDA

create CD-LIB 16 allot
create CD-SYM 64 allot
create CD-OUT 8 allot           \ shared 1-cell out-handle scratch for helpers
variable CD-H

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: NONZERO? ( n -- bool )
   0= if FALSE else TRUE then ;

: SYMBOL ( ptr u8 n -- n )
   CD-SYM >CSTR
   CD-H @ CD-SYM DLSYM ;

public

: OPEN? ( -- bool )
   CD-H @ NONZERO? if TRUE exit then
   s" libcuda.so.1" CD-LIB >CSTR
   CD-LIB RTLD-NOW DLOPEN dup CD-H !
   NONZERO? ;

: OPEN ( -- )
   OPEN? 0= if E-CUDA throw then ;

: HANDLE@ ( -- n )
   CD-H @ ;

: HANDLE0 ( n -- n )
   CUDA-HANDLE0 ;

: RC0 ( rc -- )
   CUDA-RC0 ;

FFI: CU-INIT ( n -- rc ) SYMBOL cuInit FFI;
FFI: CU-DEVICE-GET ( ptr a idx -- rc ) SYMBOL cuDeviceGet FFI;
FFI: CU-DEVICE-PRIMARY-CTX-RETAIN ( ptr a cuda-dev -- rc ) SYMBOL cuDevicePrimaryCtxRetain FFI;
FFI: CU-CTX-SET-CURRENT ( cuda-ctx -- rc ) SYMBOL cuCtxSetCurrent FFI;
FFI: CU-MODULE-LOAD ( ptr a ptr u8 -- rc ) SYMBOL cuModuleLoad FFI;
FFI: CU-MODULE-GET-FUNCTION ( ptr a cuda-mod ptr u8 -- rc ) SYMBOL cuModuleGetFunction FFI;
FFI: CU-MEM-ALLOC ( ptr a len -- rc ) SYMBOL cuMemAlloc_v2 FFI;
FFI: CU-MEM-FREE ( cuda-devptr -- rc ) SYMBOL cuMemFree_v2 FFI;
FFI: CU-MEMSET-D32 ( cuda-devptr n count -- rc ) SYMBOL cuMemsetD32_v2 FFI;
FFI: CU-MEMCPY-HTOD ( cuda-devptr ptr u8 len -- rc ) SYMBOL cuMemcpyHtoD_v2 FFI;
FFI: CU-MEMCPY-DTOH ( ptr u8 cuda-devptr len -- rc ) SYMBOL cuMemcpyDtoH_v2 FFI;
FFI: CU-FUNC-SET-BLOCK-SHAPE ( cuda-fn n n n -- rc ) SYMBOL cuFuncSetBlockShape FFI;
FFI: CU-PARAM-SET-SIZE ( cuda-fn len -- rc ) SYMBOL cuParamSetSize FFI;
FFI: CU-PARAM-SET-V ( cuda-fn idx ptr u8 len -- rc ) SYMBOL cuParamSetv FFI;
FFI: CU-LAUNCH-GRID ( cuda-fn n n -- rc ) SYMBOL cuLaunchGrid FFI;
FFI: CU-CTX-SYNCHRONIZE ( -- rc ) SYMBOL cuCtxSynchronize FFI;
FFI: CU-MODULE-UNLOAD ( cuda-mod -- rc ) SYMBOL cuModuleUnload FFI;
FFI: CU-DEVICE-PRIMARY-CTX-RELEASE ( cuda-dev -- rc ) SYMBOL cuDevicePrimaryCtxRelease FFI;

\ ---- events (GPU-side elapsed-time measurement) ----------------------------
FFI: CU-EVENT-CREATE ( ptr a n -- rc ) SYMBOL cuEventCreate FFI;
FFI: CU-EVENT-DESTROY ( cuda-event -- rc ) SYMBOL cuEventDestroy_v2 FFI;
FFI: CU-EVENT-RECORD ( cuda-event n -- rc ) SYMBOL cuEventRecord FFI;
FFI: CU-EVENT-SYNCHRONIZE ( cuda-event -- rc ) SYMBOL cuEventSynchronize FFI;
FFI: CU-EVENT-ELAPSED-TIME ( ptr a cuda-event cuda-event -- rc ) SYMBOL cuEventElapsedTime FFI;

\ ---- typed convenience helpers (named throws via HANDLE0 / RC0) -------------

: LOAD-MODULE ( ptr u8 -- cuda-mod )              \ path-cstr -> loaded module handle
   CD-OUT swap CU-MODULE-LOAD RC0
   CD-OUT @ HANDLE0 >CUDA-MOD ;

: GET-FUNCTION ( cuda-mod ptr u8 -- cuda-fn )     \ module + name-cstr -> kernel function
   {: m:cuda-mod name:ptr :}
   CD-OUT m name CU-MODULE-GET-FUNCTION RC0
   CD-OUT @ HANDLE0 >CUDA-FN ;

: DEVICE-ALLOC ( len -- cuda-devptr )             \ allocate len device bytes
   CD-OUT swap CU-MEM-ALLOC RC0
   CD-OUT @ HANDLE0 >CUDA-DEVPTR ;

: HTOD ( cuda-devptr ptr u8 len -- )              \ copy host bytes -> device
   CU-MEMCPY-HTOD RC0 ;

: DTOH ( ptr u8 cuda-devptr len -- )              \ copy device bytes -> host
   CU-MEMCPY-DTOH RC0 ;

end-package
