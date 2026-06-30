\ ffi-abi.f - checked AAPCS64 FFI calls and marshalling.
\
\ Built on the AAPCS64 trampolines from habu1.f: `ffi-call` (x0..x7 only),
\ `ffi-call-n` (integer/pointer x0..x7 plus stack spill), and `ffi-call-abi`
\ / `ffi-call-abi-r` (x0..x8, d0..d7, caller-packed stack spill, integer or
\ float return).
\
\ Marshalling uses single-threaded scratch buffers. Integer/pointer args are
\ cells in FFI-BUF; FP args are float cells in FFI-FBUF; stack spill slots are
\ prepacked cells in FFI-STACK-BUF. Kernel params are a `void**` pointer array
\ plus library-owned value cells for scalar params. Do not nest CALLk or
\ FFI-CALL* calls; finish one foreign call before preparing the next.

s" lib/errors.f" required

8 constant FFI-REG-ARGS
16 constant FFI-MAX-ARGS
16 constant FFI-MAX-KPARAMS

$3A00 constant FFI-BUF-OFF
$3A80 constant FFI-FBUF-OFF
$3AC0 constant FFI-STACK-BUF-OFF
$3B40 constant FFI-KPARAM-PBUF-OFF
$3BC0 constant FFI-KPARAM-VBUF-OFF
$3C40 constant FFI-DLBUF-OFF
$3C80 constant FFI-KPARAM#-OFF

TRUSTED: FFI-BUF ( -- ptr a )
   data-base FFI-BUF-OFF + ;

TRUSTED: FFI-FBUF ( -- ptr r )
   data-base FFI-FBUF-OFF + ;

TRUSTED: FFI-STACK-BUF ( -- ptr a )
   data-base FFI-STACK-BUF-OFF + ;

TRUSTED: FFI-KPARAM-PBUF ( -- ptr a )
   data-base FFI-KPARAM-PBUF-OFF + ;

TRUSTED: FFI-KPARAM-VBUF ( -- ptr a )
   data-base FFI-KPARAM-VBUF-OFF + ;

TRUSTED: FFI-DLBUF ( -- ptr a )
   data-base FFI-DLBUF-OFF + ;

TRUSTED: FFI-KPARAM# ( -- ptr n )
   data-base FFI-KPARAM#-OFF + ;

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
: FFI-PTR-ARG! ( ptr a n -- ) {: p:ptr idx:n :}
   p P>N idx FFI-ARG! ;
: FFI-FARG! ( r n -- ) {: v:r idx:n :}
   v idx FFI-FSLOT ! ;
: FFI-STACK! ( n n -- ) {: v:n idx:n :}
   v idx FFI-STACK-SLOT ! ;
: FFI-FSTACK! ( r n -- ) {: v:r idx:n :}
   v idx FFI-STACK-FSLOT ! ;
: FFI-X8! ( n -- )
   8 FFI-ARG! ;

\ ---- out-params -----------------------------------------------------------
: FFI-OUT@ ( ptr n -- n )
   @ ;
: FFI-OUT! ( n ptr n -- )
   ! ;

\ ---- kernelParams ---------------------------------------------------------
\ `cuLaunchKernel` wants void**: each array element points to caller-owned or
\ FFI-owned param storage. FFI-owned value cells are stable until the next
\ FFI-KPARAM-RESET or overwrite by another FFI-KPARAM-N+ sequence.
: FFI-KPARAM-COUNT ( -- n )
   FFI-KPARAM# @ ;
: FFI-KPARAM-RESET ( -- )
   0 FFI-KPARAM# ! ;
: FFI-KPARAM-CHECK ( n -- ) {: idx:n :}
   idx FFI-MAX-KPARAMS FFI-CHECK-INDEX ;
: FFI-KPARAM-PTR-SLOT ( n -- ptr n ) {: idx:n :}
   idx FFI-KPARAM-CHECK
   FFI-KPARAM-PBUF idx cells + ;
: FFI-KPARAM-VAL-SLOT ( n -- ptr n ) {: idx:n :}
   idx FFI-KPARAM-CHECK
   FFI-KPARAM-VBUF idx cells + ;
: FFI-KPARAM-BUMP ( n -- )
   1 + FFI-KPARAM# ! ;
: FFI-KPARAM+ ( ptr a -- ) {: p:ptr :}
   FFI-KPARAM-COUNT {: idx:n :}
   p P>N idx FFI-KPARAM-PTR-SLOT !
   idx FFI-KPARAM-BUMP ;
: FFI-KPARAM-N+ ( n -- ) {: v:n :}
   FFI-KPARAM-COUNT {: idx:n :}
   v idx FFI-KPARAM-VAL-SLOT !
   idx FFI-KPARAM-VAL-SLOT FFI-KPARAM+ ;
: FFI-KPARAMS ( -- ptr n n )
   FFI-KPARAM-PBUF FFI-KPARAM-COUNT ;
: FFI-KPARAMS>N ( -- n )
   FFI-KPARAM-PBUF P>N ;

\ ---- fixed-arity calls ----------------------------------------------------
\ Stack: ( arg0 .. argN-1 fn -- ret ). Args are plain cells. Prefer staged
\ pointer arguments via FFI-PTR-ARG! + FFI-CALLN; P>N is the audited low-level
\ cast for existing call sites. The trailing n is the resolved function pointer.
: CALL0 ( n -- n ) {: fn:n :}
   FFI-BUF fn ffi-call ;
: CALL1 ( n n -- n ) {: a:n fn:n :}
   a 0 FFI-ARG!  FFI-BUF fn ffi-call ;
: CALL2 ( n n n -- n ) {: a:n b:n fn:n :}
   a 0 FFI-ARG!  b 1 FFI-ARG!  FFI-BUF fn ffi-call ;
: CALL3 ( n n n n -- n ) {: a:n b:n c:n fn:n :}
   a 0 FFI-ARG!  b 1 FFI-ARG!  c 2 FFI-ARG!  FFI-BUF fn ffi-call ;
: CALL4 ( n n n n n -- n ) {: a:n b:n c:n d:n fn:n :}
   a 0 FFI-ARG!  b 1 FFI-ARG!  c 2 FFI-ARG!  d 3 FFI-ARG!
   FFI-BUF fn ffi-call ;
: CALL5 ( n n n n n n -- n ) {: a:n b:n c:n d:n e:n fn:n :}
   a 0 FFI-ARG!  b 1 FFI-ARG!  c 2 FFI-ARG!  d 3 FFI-ARG!  e 4 FFI-ARG!
   FFI-BUF fn ffi-call ;
: CALL6 ( n n n n n n n -- n ) {: a:n b:n c:n d:n e:n g:n fn:n :}
   a 0 FFI-ARG!  b 1 FFI-ARG!  c 2 FFI-ARG!  d 3 FFI-ARG!  e 4 FFI-ARG!
   g 5 FFI-ARG!  FFI-BUF fn ffi-call ;

\ ---- general arity --------------------------------------------------------
\ For >6 args: set args 0..nargs-1 with FFI-ARG!, then FFI-CALLN. Spills past 8
\ to the stack via ffi-call-n. Arity beyond the buffer is a caller error, not a
\ silent truncation.
: FFI-CALLN ( n n -- n ) {: nargs:n fn:n :}
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
: >CSTR ( ptr u8 n ptr u8 -- ) {: src:ptr u:n dst:ptr :}
   src dst u BYTE-COPY
   0 dst u + c! ;                         \ dst+u : NUL terminator
