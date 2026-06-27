\ ffi.f - checked C-ABI foreign calls.
\
\ Built on the AAPCS64 trampolines `ffi-call ( argbuf fn -- ret )` (≤8 args, fast
\ path) and `ffi-call-n ( argbuf nargs fn -- ret )` (any arity: x0..x7 plus an
\ exact, 16-byte-aligned stack spill) from habu1.f, plus the dlopen/dlsym GOT
\ slots the loader fills at startup (layout.f, elf.f). CALL0..CALL6 use the fast
\ path; FFI-CALLN handles >6 args via FFI-ARG!. Float register args (d0..d7) and
\ float/struct returns are still non-goals — the ZED/CUDA C ABIs are
\ integer/pointer-only (floats cross via buffers), so add those only when a real
\ binding needs them.
\
\ Marshalling is uniform: every arg is a machine cell (type n). Pointers cross
\ the boundary through P>N; returned handles/pointers come back through N>P. The
\ argument buffer FFI-BUF is shared single-threaded scratch, refilled per call;
\ it holds FFI-MAX-ARGS cells (8 register + 8 stack slots). DLOPEN/DLSYM use a
\ SEPARATE buffer (FFI-DLBUF) so resolving a symbol never disturbs a call's
\ marshalled args — resolve and marshal may interleave freely. The one remaining
\ rule: FFI-BUF is a single call's scratch, so do not nest CALLk/FFI-CALLN (e.g.
\ compute an arg by making another FFI call) — finish one call before the next.

16 constant FFI-MAX-ARGS
2 constant RTLD-NOW                       \ dlopen flag: resolve all symbols now

create FFI-BUF FFI-MAX-ARGS cells allot
create FFI-DLBUF 8 cells allot           \ dlopen/dlsym args, isolated from FFI-BUF

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
   path P>N FFI-DLBUF !  flags FFI-DLBUF 8 + !
   FFI-DLBUF DLOPEN-SLOT @ ffi-call ;
: DLSYM ( n ptr u8 -- n ) {: handle name:ptr :}
   handle FFI-DLBUF !  name P>N FFI-DLBUF 8 + !
   FFI-DLBUF DLSYM-SLOT @ ffi-call ;

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

\ ---- general arity --------------------------------------------------------
\ For >6 args: set args 0..nargs-1 with FFI-ARG!, then FFI-CALLN. Spills past 8
\ to the stack via ffi-call-n. Arity beyond the buffer is a caller error, not a
\ silent truncation.
: FFI-CALLN ( n n -- n ) {: nargs fn :}
   nargs FFI-MAX-ARGS > if E-FFI-ARITY throw then
   FFI-BUF nargs fn ffi-call-n ;

\ ---- C strings ------------------------------------------------------------
\ Copy a Habu byte-string into dst and NUL-terminate it, yielding a C string
\ for the callee. dst must hold at least n+1 bytes; the caller owns it.
: >CSTR ( ptr u8 n ptr u8 -- ) {: src:ptr u dst:ptr :}
   src dst u BYTE-COPY
   0 dst u + c! ;                         \ dst+u : NUL terminator
