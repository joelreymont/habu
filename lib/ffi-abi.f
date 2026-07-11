\ ffi-abi.f - checked AAPCS64 FFI calls and marshalling.
\
\ Built on the AAPCS64 trampolines from habu1.f: `ffi-call` (x0..x7 only),
\ `ffi-call-n` (integer/pointer x0..x7 plus stack spill), and `ffi-call-abi`
\ / `ffi-call-abi-r` (x0..x8, d0..d7, caller-packed stack spill, integer or
\ float return).
\
\ Marshalling uses task-DATA scratch buffers. Integer/pointer args are
\ cells in FFI-BUF; FP args are float cells in FFI-FBUF; stack spill slots are
\ prepacked cells in FFI-STACK-BUF. Kernel params are a `void**` pointer array
\ plus library-owned value cells for scalar params. Do not nest CALLk or
\ FFI-CALL* calls in one task; finish one foreign call before preparing the next.

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
$40C8 constant FFI-REG-LEN-BUF-OFF
$4148 constant FFI-STACK-LEN-BUF-OFF
$41C8 constant FFI-SCRATCH-END

: FFI-BUF ( -- ptr a )
   data-base FFI-BUF-OFF + ;

: FFI-FBUF ( -- ptr r )
   data-base FFI-FBUF-OFF + ;

: FFI-STACK-BUF ( -- ptr a )
   data-base FFI-STACK-BUF-OFF + ;

: FFI-KPARAM-PBUF ( -- ptr a )
   data-base FFI-KPARAM-PBUF-OFF + ;

: FFI-KPARAM-VBUF ( -- ptr a )
   data-base FFI-KPARAM-VBUF-OFF + ;

: FFI-DLBUF ( -- ptr a )
   data-base FFI-DLBUF-OFF + ;

: FFI-KPARAM# ( -- ptr n )
   data-base FFI-KPARAM#-OFF + ;

: FFI-REG-LEN-BUF ( -- ptr n )
   data-base FFI-REG-LEN-BUF-OFF + ;

: FFI-STACK-LEN-BUF ( -- ptr n )
   data-base FFI-STACK-LEN-BUF-OFF + ;

\ Pointer <-> cell reinterpret trusted boundary: the checker cannot know a raw
\ cell is a valid pointer, so we assert it here, once.
TRUSTED: P>N ( ptr a -- n ) ;             \ pointer  -> arg cell

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

\ ---- C strings ------------------------------------------------------------
\ Copy a Habu byte-string into dst and NUL-terminate it, yielding a C string
\ for the callee. dst must hold at least n+1 bytes; the caller owns it.
: >CSTR ( ptr u8 n ptr u8 -- ) {: src:ptr u:n dst:ptr :}
   src dst u BYTE-COPY
   0 dst u + c! ;                         \ dst+u : NUL terminator

\ Exact bindings use this sealed package surface. Legacy global marshalling
\ words remain compatible but cannot mint checked effects; raw calls remain
\ TRUSTED-only and no universal binder is published.
package FFI

: REG-LEN-SLOT ( n -- ptr n ) {: idx:n :}
   idx FFI-MAX-ARGS FFI-CHECK-INDEX
   FFI-REG-LEN-BUF idx cells + ;

: STACK-LEN-SLOT ( n -- ptr n ) {: idx:n :}
   idx FFI-MAX-ARGS FFI-CHECK-INDEX
   FFI-STACK-LEN-BUF idx cells + ;

: REG-LEN! ( n n -- ) {: len:n idx:n :}
   len 0 < if E-FFI-ARITY throw then
   len idx REG-LEN-SLOT ! ;

: STACK-LEN! ( n n -- ) {: len:n idx:n :}
   len 0 < if E-FFI-ARITY throw then
   len idx STACK-LEN-SLOT ! ;

: WRITABLE-LEN ( n -- n ) {: len:n :}
   len 0 <= if E-FFI-ARITY throw then
   len ;

public

: BUF-OFF ( -- n ) FFI-BUF-OFF ;
: KPARAM-END-OFF ( -- n ) FFI-KPARAM#-OFF CELL + ;
: SCRATCH-END ( -- n ) FFI-SCRATCH-END ;
: NOW ( -- n ) 2 ;

: >CELL ( ptr a -- n ) P>N ;

: RESET ( -- )
   FFI-MAX-ARGS 0 ?do
      0 i REG-LEN!
      0 i STACK-LEN!
   loop ;

: VALUE! ( n n -- ) {: value:n idx:n :}
   value idx FFI-ARG!
   0 idx REG-LEN! ;

: READABLE! ( ptr a n -- ) {: value:ptr idx:n :}
   value idx FFI-PTR-ARG!
   0 idx REG-LEN! ;

: WRITABLE! ( ptr a n n -- ) {: value:ptr len:n idx:n :}
   value idx FFI-PTR-ARG!
   len WRITABLE-LEN idx REG-LEN! ;

: FLOAT! ( r n -- ) FFI-FARG! ;

: STACK-VALUE! ( n n -- ) {: value:n idx:n :}
   value idx FFI-STACK!
   0 idx STACK-LEN! ;

: STACK-FLOAT! ( r n -- ) {: value:r idx:n :}
   value idx FFI-FSTACK!
   0 idx STACK-LEN! ;

: STACK-READABLE! ( ptr a n -- ) {: value:ptr idx:n :}
   value P>N idx FFI-STACK!
   0 idx STACK-LEN! ;

: STACK-WRITABLE! ( ptr a n n -- ) {: value:ptr len:n idx:n :}
   value P>N idx FFI-STACK!
   len WRITABLE-LEN idx STACK-LEN! ;

: X8-VALUE! ( n -- ) 8 VALUE! ;
: X8-READABLE! ( ptr a -- ) 8 READABLE! ;
: X8-WRITABLE! ( ptr a n -- ) 8 WRITABLE! ;

: ARGS ( -- ptr a ) FFI-BUF ;
: FLOATS ( -- ptr r ) FFI-FBUF ;
: STACK ( -- ptr a ) FFI-STACK-BUF ;
: REG-LENS ( -- ptr n ) FFI-REG-LEN-BUF ;
: STACK-LENS ( -- ptr n ) FFI-STACK-LEN-BUF ;

: OUT@ ( ptr n -- n ) FFI-OUT@ ;
: OUT! ( n ptr n -- ) FFI-OUT! ;
: KPARAM-COUNT ( -- n ) FFI-KPARAM-COUNT ;
: KPARAM-RESET ( -- ) FFI-KPARAM-RESET ;
: KPARAM+ ( ptr a -- ) FFI-KPARAM+ ;
: KPARAM-VALUE+ ( n -- ) FFI-KPARAM-N+ ;
: KPARAMS ( -- ptr n n ) FFI-KPARAMS ;
: KPARAMS>CELL ( -- n ) FFI-KPARAMS>N ;
: CSTR ( ptr u8 n ptr u8 -- ) >CSTR ;

private

TRUSTED: DLOPEN-RAW ( ptr u8 n -- n ) {: path:ptr flags:n :}
   path P>N FFI-DLBUF !
   flags FFI-DLBUF CELL + !
   0 FFI-DLBUF 2 cells + !
   0 FFI-DLBUF 3 cells + !
   FFI-DLBUF FFI-DLBUF 2 cells + 2 DLOPEN-SLOT @ ffi-call-bounded ;

TRUSTED: DLSYM-RAW ( n ptr u8 -- n ) {: handle:n name:ptr :}
   handle FFI-DLBUF !
   name P>N FFI-DLBUF CELL + !
   0 FFI-DLBUF 2 cells + !
   0 FFI-DLBUF 3 cells + !
   FFI-DLBUF FFI-DLBUF 2 cells + 2 DLSYM-SLOT @ ffi-call-bounded ;

get-current prot-wid-add

public

: DLOPEN ( ptr u8 n -- n ) DLOPEN-RAW ;
: DLSYM ( n ptr u8 -- n ) DLSYM-RAW ;

get-current prot-wid-add

end-package
