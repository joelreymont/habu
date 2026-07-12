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
variable CD-H

: OUT ( -- ptr a )
   FFI:ARGS 15 cells + ;

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: NONZERO? ( n -- bool )
   0= if FALSE else TRUE then ;

: SYMBOL ( ptr u8 n -- n )
   CD-SYM FFI:CSTR
   CD-H @ CD-SYM FFI:DLSYM
   dup 0= if E-CUDA throw then ;

public

: OPEN? ( -- bool )
   CD-H @ NONZERO? if TRUE exit then
   s" libcuda.so.1" CD-LIB FFI:CSTR
   CD-LIB FFI:NOW FFI:DLOPEN dup CD-H !
   NONZERO? ;

: OPEN ( -- )
   OPEN? 0= if E-CUDA throw then ;

: HANDLE@ ( -- n )
   CD-H @ ;

: HANDLE0 ( n -- n )
   CUDA-HANDLE0 ;

: RC0 ( rc -- )
   CUDA-RC0 ;

TRUSTED: CU-INIT ( n -- rc ) {: flags:n :}
   s" cuInit" SYMBOL {: call:n :}
   FFI:RESET  flags 0 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 1 call ffi-call-bounded ;

TRUSTED: CU-DEVICE-GET ( ptr a idx -- rc ) {: out:ptr idx:idx :}
   s" cuDeviceGet" SYMBOL {: call:n :}
   FFI:RESET  out 4 0 FFI:WRITABLE!  idx 1 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 2 call ffi-call-bounded ;

TRUSTED: CU-DEVICE-PRIMARY-CTX-RETAIN ( ptr a cuda-dev -- rc )
   {: out:ptr dev:cuda-dev :}
   s" cuDevicePrimaryCtxRetain" SYMBOL {: call:n :}
   FFI:RESET  out 8 0 FFI:WRITABLE!  dev 1 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 2 call ffi-call-bounded ;

TRUSTED: CU-CTX-SET-CURRENT ( cuda-ctx -- rc ) {: ctx:cuda-ctx :}
   s" cuCtxSetCurrent" SYMBOL {: call:n :}
   FFI:RESET  ctx 0 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 1 call ffi-call-bounded ;

TRUSTED: CU-MODULE-LOAD ( ptr a ptr u8 -- rc ) {: out:ptr path:ptr :}
   s" cuModuleLoad" SYMBOL {: call:n :}
   FFI:RESET  out 8 0 FFI:WRITABLE!  path 1 FFI:READABLE!
   FFI:ARGS FFI:REG-LENS 2 call ffi-call-bounded ;

TRUSTED: CU-MODULE-GET-FUNCTION ( ptr a cuda-mod ptr u8 -- rc )
   {: out:ptr mod:cuda-mod name:ptr :}
   s" cuModuleGetFunction" SYMBOL {: call:n :}
   FFI:RESET  out 8 0 FFI:WRITABLE!  mod 1 FFI:VALUE!  name 2 FFI:READABLE!
   FFI:ARGS FFI:REG-LENS 3 call ffi-call-bounded ;

TRUSTED: CU-MEM-ALLOC ( ptr a len -- rc ) {: out:ptr len:len :}
   s" cuMemAlloc_v2" SYMBOL {: call:n :}
   FFI:RESET  out 8 0 FFI:WRITABLE!  len 1 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 2 call ffi-call-bounded ;

TRUSTED: CU-MEM-FREE ( cuda-devptr -- rc ) {: p:cuda-devptr :}
   s" cuMemFree_v2" SYMBOL {: call:n :}
   FFI:RESET  p 0 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 1 call ffi-call-bounded ;

TRUSTED: CU-MEMSET-D32 ( cuda-devptr n count -- rc )
   {: dst:cuda-devptr value:n count:count :}
   s" cuMemsetD32_v2" SYMBOL {: call:n :}
   FFI:RESET  dst 0 FFI:VALUE!  value 1 FFI:VALUE!  count 2 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 3 call ffi-call-bounded ;

TRUSTED: CU-MEMCPY-HTOD ( cuda-devptr ptr u8 len -- rc )
   {: dst:cuda-devptr src:ptr len:len :}
   s" cuMemcpyHtoD_v2" SYMBOL {: call:n :}
   FFI:RESET  dst 0 FFI:VALUE!  src 1 FFI:READABLE!  len 2 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 3 call ffi-call-bounded ;

TRUSTED: CU-MEMCPY-DTOH ( ptr u8 cuda-devptr len -- rc )
   {: dst:ptr src:cuda-devptr len:len :}
   s" cuMemcpyDtoH_v2" SYMBOL {: call:n :}
   FFI:RESET  dst len 0 FFI:WRITABLE!  src 1 FFI:VALUE!  len 2 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 3 call ffi-call-bounded ;

TRUSTED: CU-FUNC-SET-BLOCK-SHAPE ( cuda-fn n n n -- rc )
   {: fn:cuda-fn x:n y:n z:n :}
   s" cuFuncSetBlockShape" SYMBOL {: call:n :}
   FFI:RESET  fn 0 FFI:VALUE!  x 1 FFI:VALUE!  y 2 FFI:VALUE!  z 3 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 4 call ffi-call-bounded ;

TRUSTED: CU-PARAM-SET-SIZE ( cuda-fn len -- rc ) {: fn:cuda-fn len:len :}
   s" cuParamSetSize" SYMBOL {: call:n :}
   FFI:RESET  fn 0 FFI:VALUE!  len 1 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 2 call ffi-call-bounded ;

TRUSTED: CU-PARAM-SET-V ( cuda-fn idx ptr u8 len -- rc )
   {: fn:cuda-fn off:idx src:ptr len:len :}
   s" cuParamSetv" SYMBOL {: call:n :}
   FFI:RESET  fn 0 FFI:VALUE!  off 1 FFI:VALUE!  src 2 FFI:READABLE!  len 3 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 4 call ffi-call-bounded ;

TRUSTED: CU-LAUNCH-GRID ( cuda-fn n n -- rc ) {: fn:cuda-fn x:n y:n :}
   s" cuLaunchGrid" SYMBOL {: call:n :}
   FFI:RESET  fn 0 FFI:VALUE!  x 1 FFI:VALUE!  y 2 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 3 call ffi-call-bounded ;

TRUSTED: CU-CTX-SYNCHRONIZE ( -- rc )
   s" cuCtxSynchronize" SYMBOL {: call:n :}
   FFI:RESET
   FFI:ARGS FFI:REG-LENS 0 call ffi-call-bounded ;

TRUSTED: CU-MODULE-UNLOAD ( cuda-mod -- rc ) {: mod:cuda-mod :}
   s" cuModuleUnload" SYMBOL {: call:n :}
   FFI:RESET  mod 0 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 1 call ffi-call-bounded ;

TRUSTED: CU-DEVICE-PRIMARY-CTX-RELEASE ( cuda-dev -- rc ) {: dev:cuda-dev :}
   s" cuDevicePrimaryCtxRelease" SYMBOL {: call:n :}
   FFI:RESET  dev 0 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 1 call ffi-call-bounded ;

\ ---- events (GPU-side elapsed-time measurement) ----------------------------
TRUSTED: CU-EVENT-CREATE ( ptr a n -- rc ) {: out:ptr flags:n :}
   s" cuEventCreate" SYMBOL {: call:n :}
   FFI:RESET  out 8 0 FFI:WRITABLE!  flags 1 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 2 call ffi-call-bounded ;

TRUSTED: CU-EVENT-DESTROY ( cuda-event -- rc ) {: event:cuda-event :}
   s" cuEventDestroy_v2" SYMBOL {: call:n :}
   FFI:RESET  event 0 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 1 call ffi-call-bounded ;

TRUSTED: CU-EVENT-RECORD ( cuda-event n -- rc ) {: event:cuda-event stream:n :}
   s" cuEventRecord" SYMBOL {: call:n :}
   FFI:RESET  event 0 FFI:VALUE!  stream 1 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 2 call ffi-call-bounded ;

TRUSTED: CU-EVENT-SYNCHRONIZE ( cuda-event -- rc ) {: event:cuda-event :}
   s" cuEventSynchronize" SYMBOL {: call:n :}
   FFI:RESET  event 0 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 1 call ffi-call-bounded ;

TRUSTED: CU-EVENT-ELAPSED-TIME ( ptr a cuda-event cuda-event -- rc )
   {: out:ptr start:cuda-event stop:cuda-event :}
   s" cuEventElapsedTime" SYMBOL {: call:n :}
   FFI:RESET  out 4 0 FFI:WRITABLE!  start 1 FFI:VALUE!  stop 2 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 3 call ffi-call-bounded ;

\ Historical CUDA C spellings remain package methods.  The package is sealed
\ below, so no later source can redirect them or add competing bindings.
: CUINIT ( n -- rc ) CU-INIT ;
: CUDEVICEGET ( ptr a idx -- rc ) CU-DEVICE-GET ;
: CUDEVICEPRIMARYCTXRETAIN ( ptr a cuda-dev -- rc ) CU-DEVICE-PRIMARY-CTX-RETAIN ;
: CUCTXSETCURRENT ( cuda-ctx -- rc ) CU-CTX-SET-CURRENT ;
: CUMODULELOAD ( ptr a ptr u8 -- rc ) CU-MODULE-LOAD ;
: CUMODULEGETFUNCTION ( ptr a cuda-mod ptr u8 -- rc ) CU-MODULE-GET-FUNCTION ;
: CUMEMALLOC ( ptr a len -- rc ) CU-MEM-ALLOC ;
: CUMEMFREE ( cuda-devptr -- rc ) CU-MEM-FREE ;
: CUMEMSETD32 ( cuda-devptr n count -- rc ) CU-MEMSET-D32 ;
: CUMEMCPYHTOD ( cuda-devptr ptr u8 len -- rc ) CU-MEMCPY-HTOD ;
: CUMEMCPYDTOH ( ptr u8 cuda-devptr len -- rc ) CU-MEMCPY-DTOH ;
: CUFUNCSETBLOCKSHAPE ( cuda-fn n n n -- rc ) CU-FUNC-SET-BLOCK-SHAPE ;
: CUPARAMSETSIZE ( cuda-fn len -- rc ) CU-PARAM-SET-SIZE ;
: CUPARAMSETV ( cuda-fn idx ptr u8 len -- rc ) CU-PARAM-SET-V ;
: CULAUNCHGRID ( cuda-fn n n -- rc ) CU-LAUNCH-GRID ;
: CUCTXSYNCHRONIZE ( -- rc ) CU-CTX-SYNCHRONIZE ;
: CUMODULEUNLOAD ( cuda-mod -- rc ) CU-MODULE-UNLOAD ;
: CUDEVICEPRIMARYCTXRELEASE ( cuda-dev -- rc ) CU-DEVICE-PRIMARY-CTX-RELEASE ;

\ ---- typed convenience helpers (named throws via HANDLE0 / RC0) -------------

: LOAD-MODULE ( ptr u8 -- cuda-mod )              \ path-cstr -> loaded module handle
   OUT swap CU-MODULE-LOAD RC0
   OUT @ HANDLE0 >CUDA-MOD ;

: GET-FUNCTION ( cuda-mod ptr u8 -- cuda-fn )     \ module + name-cstr -> kernel function
   {: m:cuda-mod name:ptr :}
   OUT m name CU-MODULE-GET-FUNCTION RC0
   OUT @ HANDLE0 >CUDA-FN ;

: DEVICE-ALLOC ( len -- cuda-devptr )             \ allocate len device bytes
   OUT swap CU-MEM-ALLOC RC0
   OUT @ HANDLE0 >CUDA-DEVPTR ;

: HTOD ( cuda-devptr ptr u8 len -- )              \ copy host bytes -> device
   CU-MEMCPY-HTOD RC0 ;

: DTOH ( ptr u8 cuda-devptr len -- )              \ copy device bytes -> host
   CU-MEMCPY-DTOH RC0 ;

private

get-current prot-wid-add

public

get-current prot-wid-add

;package
