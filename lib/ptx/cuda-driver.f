\ cuda-driver.f - fail-closed CUDA Driver FFI helpers for PTX device proofs.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/ffi.f

package CUDA

64 constant NAME-CAP
FS-PATHZ-CAP constant CSTR-CAP

create NAME-BUF NAME-CAP allot
create CSTR-BUF CSTR-CAP allot

variable H
variable LAST-RC

: CHECK-CSTR-CAP ( n -- )
   1 + CSTR-CAP > if E-FS-PATH throw then ;

: CSTR! ( ptr u8 n -- ptr u8 )
   dup CHECK-CSTR-CAP
   CSTR-BUF >CSTR
   CSTR-BUF ;

: NAME! ( ptr u8 n -- ptr u8 )
   dup 1 + NAME-CAP > if E-FS-PATH throw then
   NAME-BUF >CSTR
   NAME-BUF ;

public

: RESET ( -- )
   0 H !
   0 LAST-RC ! ;

: LAST-RC@ ( -- n )
   LAST-RC @ ;

: CHECK-RC ( n -- )
   {: rc:n :}
   rc LAST-RC !
   rc 0 <> if E-PTX-CUDA-RC throw then ;

: CHECK-LIB ( n -- n )
   dup 0= if E-PTX-CUDA-DLOPEN throw then ;

: CHECK-SYM ( n -- n )
   dup 0= if E-PTX-CUDA-DLSYM throw then ;

: EXPECT-GOLDEN ( n n -- )
   <> if E-PTX-DEVICE-GOLDEN throw then ;

: OPEN-LIB ( ptr u8 n -- )
   CSTR!
   RTLD-NOW DLOPEN CHECK-LIB H ! ;

: OPEN ( -- )
   s" libcuda.so.1" OPEN-LIB ;

: SYM ( ptr u8 n -- n )
   H @ 0= if E-PTX-CUDA-DLOPEN throw then
   NAME!
   H @ swap DLSYM CHECK-SYM ;

: CALL0-RC ( ptr u8 n -- )
   SYM CALL0 CHECK-RC ;

: CALL1-RC ( n ptr u8 n -- )
   SYM CALL1 CHECK-RC ;

: CALL2-RC ( n n ptr u8 n -- )
   SYM CALL2 CHECK-RC ;

: CALL3-RC ( n n n ptr u8 n -- )
   SYM CALL3 CHECK-RC ;

: CALL4-RC ( n n n n ptr u8 n -- )
   SYM CALL4 CHECK-RC ;

: INIT ( -- )
   OPEN
   0 s" cuInit" CALL1-RC ;

: DEVICE-GET ( ptr a -- )
   P>N 0 s" cuDeviceGet" CALL2-RC ;

: PRIMARY-CTX-RETAIN ( ptr a n -- )
   {: out:ptr dev:n :}
   out P>N dev s" cuDevicePrimaryCtxRetain" CALL2-RC ;

: CTX-CURRENT! ( n -- )
   s" cuCtxSetCurrent" CALL1-RC ;

: PRIMARY-CTX-RELEASE ( n -- )
   s" cuDevicePrimaryCtxRelease" CALL1-RC ;

: LOAD-MODULE ( ptr u8 n ptr a -- )
   {: out:ptr :}
   2dup FILE? 0= if E-PTX-CUDA-CUBIN throw then
   FS-PATHZ {: z:ptr :}
   out P>N z P>N s" cuModuleLoad" CALL2-RC ;

: MODULE-FUNCTION ( n ptr u8 n ptr a -- )
   {: mod:n name:ptr nameu:n out:ptr :}
   name nameu CSTR! {: z:ptr :}
   out P>N mod z P>N s" cuModuleGetFunction" CALL3-RC ;

: UNLOAD-MODULE ( n -- )
   dup 0 <> if s" cuModuleUnload" CALL1-RC else drop then ;

: DEVICE-ALLOC ( n ptr a -- )
   {: bytes:n out:ptr :}
   out P>N bytes s" cuMemAlloc_v2" CALL2-RC ;

: DEVICE-FREE ( n -- )
   dup 0 <> if s" cuMemFree_v2" CALL1-RC else drop then ;

: MEMSET32 ( n n n -- )
   s" cuMemsetD32_v2" CALL3-RC ;

: HTOD ( n ptr u8 n -- )
   {: dev:n a:ptr u:n :}
   dev a P>N u s" cuMemcpyHtoD_v2" CALL3-RC ;

: DTOH ( ptr u8 n n -- )
   {: a:ptr dev:n u:n :}
   a P>N dev u s" cuMemcpyDtoH_v2" CALL3-RC ;

: BLOCK-SHAPE ( n n n n -- )
   s" cuFuncSetBlockShape" CALL4-RC ;

: PARAM-SIZE ( n n -- )
   s" cuParamSetSize" CALL2-RC ;

: PARAM! ( n n ptr a n -- )
   {: func:n off:n addr:ptr bytes:n :}
   func off addr P>N bytes s" cuParamSetv" CALL4-RC ;

: PARAM-PTR! ( n n ptr a -- )
   8 PARAM! ;

: PARAM-U32! ( n n ptr a -- )
   4 PARAM! ;

: LAUNCH-GRID ( n n n -- )
   s" cuLaunchGrid" CALL3-RC ;

: SYNC ( -- )
   0 s" cuCtxSynchronize" CALL1-RC ;

end-package
