\ ffi.f - checked C-ABI foreign calls with dynamic loading.

: FFI-DEPS ( -- )
   s" E-FFI-ARITY" XREF-FIND 0= if s" lib/errors.f" included then ;

FFI-DEPS

include lib/ffi-abi.f

2 constant RTLD-NOW                       \ dlopen flag: resolve all symbols now

\ ---- dynamic loading ------------------------------------------------------
\ DLOPEN returns 0 on failure; DLSYM returns 0 for a missing symbol. Callers
\ that must not proceed on failure check the handle/fn against 0 themselves.
: DLOPEN ( ptr u8 n -- n ) {: path:ptr flags :}
   path P>N FFI-DLBUF !  flags FFI-DLBUF 8 + !
   FFI-DLBUF DLOPEN-SLOT @ ffi-call ;
: DLSYM ( n ptr u8 -- n ) {: handle name:ptr :}
   handle FFI-DLBUF !  name P>N FFI-DLBUF 8 + !
   FFI-DLBUF DLSYM-SLOT @ ffi-call ;
