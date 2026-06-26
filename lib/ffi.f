\ ffi.f - checked C-ABI foreign calls.
\
\ Built on the AAPCS64 trampoline `ffi-call ( argbuf fn -- ret )` (habu1.f) and
\ the dlopen/dlsym GOT slots the loader fills at startup (layout.f, elf.f). The
\ trampoline loads 8 cells from `argbuf` into x0..x7, `blr fn`, returns x0 — so
\ this covers up to 8 integer/pointer args and an integer/pointer return. Float
\ register args (d0..d7), >8 stack-spilled args, and float/struct returns are
\ deliberate non-goals here; add them when a binding needs them, not before.
\
\ Marshalling is uniform: every arg is a machine cell (type n). Pointers cross
\ the boundary through P>N; returned handles/pointers come back through N>P. The
\ argument buffer FFI-BUF is shared single-threaded scratch, refilled per call.

8 constant FFI-MAX-ARGS
2 constant RTLD-NOW                       \ dlopen flag: resolve all symbols now

create FFI-BUF FFI-MAX-ARGS cells allot

\ Pointer <-> cell reinterpret. The only trusted boundary in this file: the
\ checker cannot know a raw cell is a valid pointer, so we assert it here, once.
TRUSTED: P>N ( ptr a -- n ) ;             \ pointer  -> arg cell
TRUSTED: N>P ( n -- ptr u8 ) ;            \ ret cell -> byte pointer

\ ---- argument marshalling -------------------------------------------------
\ Slot i lives at FFI-BUF + i*8. Args are written low-index-first by the CALLk
\ words below; unused slots keep stale values but the (non-variadic) callee
\ never reads past its declared arity.
: FFI-SLOT ( n -- ptr n ) {: idx :}  FFI-BUF idx 8 * + ;
: FFI-ARG! ( n n -- ) {: v idx :}  v idx FFI-SLOT ! ;

\ ---- dynamic loading ------------------------------------------------------
\ DLOPEN returns 0 on failure; DLSYM returns 0 for a missing symbol. Callers
\ that must not proceed on failure check the handle/fn against 0 themselves.
: DLOPEN ( ptr u8 n -- n ) {: path:ptr flags :}
   path P>N 0 FFI-ARG!  flags 1 FFI-ARG!
   FFI-BUF DLOPEN-SLOT @ ffi-call ;
: DLSYM ( n ptr u8 -- n ) {: handle name:ptr :}
   handle 0 FFI-ARG!  name P>N 1 FFI-ARG!
   FFI-BUF DLSYM-SLOT @ ffi-call ;

\ ---- fixed-arity calls ----------------------------------------------------
\ Stack: ( arg0 .. argN-1 fn -- ret ). Args are plain cells; marshal pointers
\ with P>N at the call site. The trailing n is the resolved function pointer.
: CALL0 ( n -- n ) {: fn :}
   FFI-BUF fn ffi-call ;
: CALL1 ( n n -- n ) {: a fn :}
   a 0 FFI-ARG!  FFI-BUF fn ffi-call ;
: CALL2 ( n n n -- n ) {: a b fn :}
   a 0 FFI-ARG!  b 1 FFI-ARG!  FFI-BUF fn ffi-call ;
: CALL3 ( n n n n -- n ) {: a b c fn :}
   a 0 FFI-ARG!  b 1 FFI-ARG!  c 2 FFI-ARG!  FFI-BUF fn ffi-call ;
: CALL4 ( n n n n n -- n ) {: a b c d fn :}
   a 0 FFI-ARG!  b 1 FFI-ARG!  c 2 FFI-ARG!  d 3 FFI-ARG!
   FFI-BUF fn ffi-call ;
: CALL5 ( n n n n n n -- n ) {: a b c d e fn :}
   a 0 FFI-ARG!  b 1 FFI-ARG!  c 2 FFI-ARG!  d 3 FFI-ARG!  e 4 FFI-ARG!
   FFI-BUF fn ffi-call ;
: CALL6 ( n n n n n n n -- n ) {: a b c d e g fn :}
   a 0 FFI-ARG!  b 1 FFI-ARG!  c 2 FFI-ARG!  d 3 FFI-ARG!  e 4 FFI-ARG!
   g 5 FFI-ARG!  FFI-BUF fn ffi-call ;

\ ---- C strings ------------------------------------------------------------
\ Copy a Habu byte-string into dst and NUL-terminate it, yielding a C string
\ for the callee. dst must hold at least n+1 bytes; the caller owns it.
: >CSTR ( ptr u8 n ptr u8 -- ) {: src:ptr u dst:ptr :}
   src dst u BYTE-COPY
   0 dst u + c! ;                         \ dst+u : NUL terminator
