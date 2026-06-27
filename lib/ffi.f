\ ffi.f - checked C-ABI foreign calls.
\
\ Built on the AAPCS64 trampolines from habu1.f: `ffi-call` (x0..x7 only),
\ `ffi-call-n` (integer/pointer x0..x7 plus stack spill), and `ffi-call-abi`
\ / `ffi-call-abi-r` (x0..x8, d0..d7, caller-packed stack spill, integer or
\ float return). The dlopen/dlsym GOT slots are filled by the loader at startup
\ (layout.f, elf.f).
\
\ Marshalling uses single-threaded scratch buffers. Integer/pointer args are
\ cells in FFI-BUF; FP args are float cells in FFI-FBUF; stack spill slots are
\ prepacked cells in FFI-STACK-BUF. DLOPEN/DLSYM use a separate FFI-DLBUF so
\ resolving a symbol never disturbs marshalled args. Do not nest CALLk or
\ FFI-CALL* calls; finish one foreign call before preparing the next.

8 constant FFI-REG-ARGS
16 constant FFI-MAX-ARGS
2 constant RTLD-NOW                       \ dlopen flag: resolve all symbols now

create FFI-BUF FFI-MAX-ARGS cells allot
create FFI-FBUF FFI-REG-ARGS cells allot
create FFI-STACK-BUF FFI-MAX-ARGS cells allot
create FFI-DLBUF 8 cells allot           \ dlopen/dlsym args, isolated from FFI-BUF

\ Pointer <-> cell reinterpret. The only trusted boundary in this file: the
\ checker cannot know a raw cell is a valid pointer, so we assert it here, once.
TRUSTED: P>N ( ptr a -- n ) ;             \ pointer  -> arg cell
TRUSTED: N>P ( n -- ptr u8 ) ;            \ ret cell -> byte pointer

\ ---- argument marshalling -------------------------------------------------
\ Integer slot 8 is x8, the AAPCS64 indirect-result register. Stack slots are
\ copied to the C stack exactly as prepacked by the caller.
: FFI-CHECK-INDEX ( n n -- ) {: idx:n cap:n :}
   idx 0 < if E-FFI-ARITY throw then
   idx cap >= if E-FFI-ARITY throw then ;
: FFI-CHECK-COUNT ( n n -- ) {: count:n cap:n :}
   count 0 < if E-FFI-ARITY throw then
   count cap > if E-FFI-ARITY throw then ;
: FFI-SLOT ( n -- ptr n ) {: idx:n :}
   idx FFI-MAX-ARGS FFI-CHECK-INDEX
   FFI-BUF idx cells + ;
: FFI-FSLOT ( n -- ptr r ) {: idx:n :}
   idx FFI-REG-ARGS FFI-CHECK-INDEX
   FFI-FBUF idx cells + ;
: FFI-STACK-SLOT ( n -- ptr n ) {: idx:n :}
   idx FFI-MAX-ARGS FFI-CHECK-INDEX
   FFI-STACK-BUF idx cells + ;
: FFI-STACK-FSLOT ( n -- ptr r ) {: idx:n :}
   idx FFI-MAX-ARGS FFI-CHECK-INDEX
   FFI-STACK-BUF idx cells + ;
: FFI-ARG! ( n n -- ) {: v:n idx:n :}
   v idx FFI-SLOT ! ;
: FFI-FARG! ( r n -- ) {: v:r idx:n :}
   v idx FFI-FSLOT ! ;
: FFI-STACK! ( n n -- ) {: v:n idx:n :}
   v idx FFI-STACK-SLOT ! ;
: FFI-FSTACK! ( r n -- ) {: v:r idx:n :}
   v idx FFI-STACK-FSLOT ! ;
: FFI-X8! ( n -- )
   8 FFI-ARG! ;

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
   nargs FFI-MAX-ARGS FFI-CHECK-COUNT
   FFI-BUF nargs fn ffi-call-n ;
: FFI-CALLABI ( n n -- n ) {: stackcells:n fn:n :}
   stackcells FFI-MAX-ARGS FFI-CHECK-COUNT
   FFI-BUF FFI-FBUF FFI-STACK-BUF stackcells fn ffi-call-abi ;
: FFI-CALLABI-R ( n n -- r ) {: stackcells:n fn:n :}
   stackcells FFI-MAX-ARGS FFI-CHECK-COUNT
   FFI-BUF FFI-FBUF FFI-STACK-BUF stackcells fn ffi-call-abi-r ;

\ ---- C strings ------------------------------------------------------------
\ Copy a Habu byte-string into dst and NUL-terminate it, yielding a C string
\ for the callee. dst must hold at least n+1 bytes; the caller owns it.
: >CSTR ( ptr u8 n ptr u8 -- ) {: src:ptr u dst:ptr :}
   src dst u BYTE-COPY
   0 dst u + c! ;                         \ dst+u : NUL terminator
