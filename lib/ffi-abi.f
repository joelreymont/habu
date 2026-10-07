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
s" lib/string.f" required
s" lib/image-lifecycle.f" required
s" lib/le.f" required                     \ the four-byte C int this package reads back
s" lib/adt/option.f" required             \ option<n> - the version token VERSIONED-LIBRARY parses
s" lib/codegen.f" required                \ the rendered library name and the declarer's buffers

package FFI

8 constant FFI-REG-ARGS
16 constant FFI-MAX-ARGS
16 constant FFI-MAX-KPARAMS
2 constant FFI-RTLD-NOW                   \ dlopen RTLD_NOW: bind every symbol at load

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

: FFI-BUF ( -- ptr n )
   data-base FFI-BUF-OFF + ;

: FFI-FBUF ( -- ptr r )
   data-base FFI-FBUF-OFF + ;

\ The spill area is prepacked: slot `idx` carries an integer for an integer
\ argument and a float for a float one, and only the signature being marshalled
\ says which. Two accessors declare the same offset at the two pointees the two
\ slot words really reach, the way FFI-BUF and FFI-FBUF declare the register
\ areas; a single `( -- ptr a )` let the CALLER pick the element type of engine
\ DATA, which is the mint this rule removes.
: FFI-STACK-BUF ( -- ptr n )
   data-base FFI-STACK-BUF-OFF + ;

: FFI-STACK-FBUF ( -- ptr r )
   data-base FFI-STACK-BUF-OFF + ;

: FFI-KPARAM-PBUF ( -- ptr n )
   data-base FFI-KPARAM-PBUF-OFF + ;

: FFI-KPARAM-VBUF ( -- ptr n )
   data-base FFI-KPARAM-VBUF-OFF + ;

: FFI-DLBUF ( -- ptr n )
   data-base FFI-DLBUF-OFF + ;

: FFI-KPARAM# ( -- ptr n )
   data-base FFI-KPARAM#-OFF + ;

: FFI-REG-LEN-BUF ( -- ptr n )
   data-base FFI-REG-LEN-BUF-OFF + ;

: FFI-STACK-LEN-BUF ( -- ptr n )
   data-base FFI-STACK-LEN-BUF-OFF + ;

\ Pointer <-> cell reinterpret. An argument buffer slot holds a pointer as a
\ cell, and a foreign function that returns an address hands back a cell that
\ has to be read as bytes. The two identity axioms FFI-PTR>CELL / FFI-CELL>PTR
\ are declared in src/core/checker.f with an owner-private row that admits this
\ package and no other scope. `( ptr a -- n )` is no CAST: (a type-variable
\ pointee is refused as possibly linear, E-CAST-LINEAR), and `( n -- ptr u8 )`
\ is the private class mint a CAST: also declares. CELL>PTR stays private: a
\ foreign address carries no length, so only a word that knows the callee's
\ width may publish one.
: PTR>CELL ( ptr a -- n ) FFI-PTR>CELL ;
: CELL>PTR ( n -- ptr u8 ) FFI-CELL>PTR ;

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
   FFI-STACK-FBUF idx cells + ;
: FFI-ARG! ( n n -- ) {: v:n idx:n :}
   v idx FFI-SLOT ! ;
: FFI-PTR-ARG! ( ptr a n -- ) {: p:ptr idx:n :}
   p PTR>CELL idx FFI-ARG! ;
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
   p PTR>CELL idx FFI-KPARAM-PTR-SLOT !
   idx FFI-KPARAM-BUMP ;
: FFI-KPARAM-N+ ( n -- ) {: v:n :}
   FFI-KPARAM-COUNT {: idx:n :}
   v idx FFI-KPARAM-VAL-SLOT !
   idx FFI-KPARAM-VAL-SLOT FFI-KPARAM+ ;
: FFI-KPARAMS ( -- ptr n n )
   FFI-KPARAM-PBUF FFI-KPARAM-COUNT ;
: FFI-KPARAMS>N ( -- n )
   FFI-KPARAM-PBUF PTR>CELL ;

\ ---- C strings ------------------------------------------------------------
\ Copy a Habu byte-string into dst and NUL-terminate it, yielding a C string
\ for the callee. dst must hold at least n+1 bytes; the caller owns it.
: COPY-CSTR ( ptr u8 n ptr u8 -- ) {: src:ptr u:n dst:ptr :}
   u 0 < if E-FFI-SYNTAX throw then
   src dst u BYTE-COPY
   0 dst u + c! ;                         \ dst+u : NUL terminator

\ Exact bindings use this sealed package surface. Raw calls remain TRUSTED-only
\ and no universal binder is published.

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

\ Exact package-private dlopen/dlsym bindings: paths and symbols are read-only,
\ while handles and flags are scalar. Checked bodies over the owner-private
\ ffi-call-bounded row; the loader's own two GOT slots hold the addresses.
: DLOPEN-RAW ( ptr u8 n -- n ) {: path:ptr flags:n :}
   path PTR>CELL FFI-DLBUF !
   flags FFI-DLBUF CELL + !
   0 FFI-DLBUF 2 cells + !
   0 FFI-DLBUF 3 cells + !
   FFI-DLBUF FFI-DLBUF 2 cells + 2 DLOPEN-SLOT @ ffi-call-bounded ;

: DLSYM-RAW ( n ptr u8 -- n ) {: handle:n name:ptr :}
   handle FFI-DLBUF !
   name PTR>CELL FFI-DLBUF CELL + !
   0 FFI-DLBUF 2 cells + !
   0 FFI-DLBUF 3 cells + !
   FFI-DLBUF FFI-DLBUF 2 cells + 2 DLSYM-SLOT @ ffi-call-bounded ;

\ ---- declared foreign functions -------------------------------------------
\ FUNCTION: (the declarer below this package) registers one row here and the
\ word it generates names that row's INDEX. A checked caller therefore reaches
\ only a symbol some declaration published: the function address is never a
\ value on the stack, so the sealed surface above still publishes no universal
\ binder. A row resolves its symbol on the first call and caches the address;
\ library 0 is the process itself (RTLD_DEFAULT, acquiring no reference), any
\ other index is a path this package dlopens once.
\
\ THE TABLE IS SIZED FOR A SERVER IMAGE, not for a library or two. One process
\ legitimately binds five or six: the Tender server holds TCP4's 11 rows,
\ libcurl's 15, libpq's 18, libcrypto's 19 and task's 5 at once, 68 before it
\ declares anything of its own, and each later milestone adds foreign surface.
\ A row costs FN-NAME-CAP bytes of symbol plus four cells (address, library,
\ argc, spill count) = $50 bytes, so the table is FN-MAX * $50 = $5000 bytes of image
\ data. A declaration past the last row is E-FFI-TABLE-FULL, not a wrap or a
\ silent drop.

$100 constant FN-MAX                      \ $100 rows * $50 bytes = $5000 bytes
$30 constant FN-NAME-CAP                  \ NUL-terminated C symbol
FN-MAX constant LIB-MAX                 \ each callable row may name a distinct library
PATH-CAP 1 + constant LIB-PATH-CAP      \ a library path of PATH-CAP bytes and its NUL
$20 constant LIB-BASE-CAP                 \ the base name LIBRARY-NAME$ renders around
9999 constant LIB-VERSION-MAX             \ ... and its soname version

create FN-NAMES FN-MAX FN-NAME-CAP * allot
create FN-ADDRS FN-MAX cells allot
create FN-LIBS FN-MAX cells allot
create FN-ARGCS FN-MAX cells allot
create FN-SPILLS FN-MAX cells allot
create LIB-PATHS LIB-MAX LIB-PATH-CAP * allot
create LIB-LENS LIB-MAX cells allot
create LIB-HANDLES LIB-MAX cells allot

variable FN-N
variable LIB-N
variable FN-REGISTERED

: FN-CHECK ( n -- ) {: idx:n :}
   idx FN-MAX FFI-CHECK-INDEX ;

: FN-NAME ( n -- ptr u8 ) {: idx:n :}
   idx FN-CHECK
   FN-NAMES idx FN-NAME-CAP * + ;

: FN-ADDR@ ( n -- n ) {: idx:n :}
   idx FN-CHECK
   FN-ADDRS idx cells + @ ;

: FN-ADDR! ( n n -- ) {: value:n idx:n :}
   idx FN-CHECK
   value FN-ADDRS idx cells + ! ;

: FN-LIB@ ( n -- n ) {: idx:n :}
   idx FN-CHECK
   FN-LIBS idx cells + @ ;

: FN-SPILL@ ( n -- n ) {: idx:n :}
   idx FN-CHECK FN-SPILLS idx cells + @ ;

: FN-ARGC@ ( n -- n ) {: idx:n :}
   idx FN-CHECK
   FN-ARGCS idx cells + @ ;

: LIB-CHECK ( n -- ) {: idx:n :}
   idx LIB-MAX FFI-CHECK-INDEX ;

: LIB-PATH ( n -- ptr u8 ) {: idx:n :}
   idx LIB-CHECK
   LIB-PATHS idx LIB-PATH-CAP * + ;

: LIB-LEN@ ( n -- n ) {: idx:n :}
   idx LIB-CHECK
   LIB-LENS idx cells + @ ;

: LIB-LEN! ( n n -- ) {: u:n idx:n :}
   idx LIB-CHECK
   u LIB-LENS idx cells + ! ;

: LIB-HANDLE@ ( n -- n ) {: idx:n :}
   idx LIB-CHECK
   LIB-HANDLES idx cells + @ ;

: LIB-HANDLE! ( n n -- ) {: value:n idx:n :}
   idx LIB-CHECK
   value LIB-HANDLES idx cells + ! ;

\ Every call stages its own arguments in the register and spill buffers, the
\ dlopen/dlsym buffer and the two length buffers, so between calls they hold
\ only the last call's values: proc_pidpath's process id, for one, which made
\ two --repl builds of one program differ (tools/hb-build-repl-twin-test.f).
\ Kernel params are not per call; they stay until FFI-KPARAM-RESET.
: FORGET-STAGED ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do 0 a i + c! loop ;

\ Library 0 is RTLD_DEFAULT and has no handle to acquire.
: PROCESS-HANDLE ( -- n )
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if 0 exit then
   HB-TARGET-MACOS? if -2 exit then
   E-FFI-LIBRARY throw ;

: LIB-RESOLVE ( n -- n ) {: lib:n :}
   lib 0= if PROCESS-HANDLE exit then
   lib 1 - {: slot:n :}
   slot LIB-HANDLE@ dup 0 <> if exit then
   drop
   slot LIB-PATH FFI-RTLD-NOW DLOPEN-RAW
   dup 0= if E-FFI-DLSYM throw then
   dup slot LIB-HANDLE! ;

variable ERRNO-FN-CELL
variable DLCLOSE-FN-CELL

: ERRNO-FN ( -- n ) ERRNO-FN-CELL @ ;

\ Resolution is lazy so a declaration never calls the loader, and a missing
\ symbol is a named failure at the first call rather than at load time.
\ Every first resolution resolves the errno accessor first, so once a row's call
\ returns, ERRNO's own lookup is the cached path and nothing runs the loader
\ between the call and its errno read: the loader may change errno even when it
\ succeeds. This lookup registers nothing; FN-RESOLVE below arms FFI's cleanup
\ before it lets the loader acquire anything.
: FN-LOOKUP ( n -- n ) {: idx:n :}
   idx FN-ADDR@ dup 0 <> if exit then
   drop
   idx ERRNO-FN <> if ERRNO-FN RECURSE drop then
   idx FN-LIB@ LIB-RESOLVE idx FN-NAME DLSYM-RAW
   dup 0= if E-FFI-DLSYM throw then
   dup idx FN-ADDR! ;

: NAME-ROOM ( n -- n ) {: u:n :}
   u 0 <= if E-FFI-SYNTAX throw then
   u FN-NAME-CAP 1 - > if E-FFI-SYNTAX throw then
   u ;

: PATH-ROOM ( n -- n ) {: u:n :}
   u 0 <= if E-FFI-SYNTAX throw then
   u LIB-PATH-CAP 1 - > if E-FFI-SYNTAX throw then
   u ;

\ LIBRARY-NAME$'s two inputs, refused by the same code an over-long path is.
: BASE-CHECK ( n -- ) {: u:n :}
   u 0 <= if E-FFI-SYNTAX throw then
   u LIB-BASE-CAP > if E-FFI-SYNTAX throw then ;

: VERSION-CHECK ( n -- ) {: version:n :}
   version 0 < if E-FFI-SYNTAX throw then
   version LIB-VERSION-MAX > if E-FFI-SYNTAX throw then ;

public

: BUF-OFF ( -- n ) FFI-BUF-OFF ;
: KPARAM-END-OFF ( -- n ) FFI-KPARAM#-OFF CELL + ;
: SCRATCH-END ( -- n ) FFI-SCRATCH-END ;
: NOW ( -- n ) FFI-RTLD-NOW ;

\ dlopen RTLD_GLOBAL, for `FFI:NOW FFI:GLOBAL or`: the library's symbols join
\ process-wide resolution (RTLD_DEFAULT, which PROCESS-SYMBOLS resolves
\ through). <dlfcn.h> spells it 0x100 in glibc and 0x8 on macOS.
: GLOBAL ( -- n ) HB-TARGET-MACOS? if $8 else $100 then ;

: >CELL ( ptr a -- n ) PTR>CELL ;

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
   value PTR>CELL idx FFI-STACK!
   0 idx STACK-LEN! ;

: STACK-WRITABLE! ( ptr a n n -- ) {: value:ptr len:n idx:n :}
   value PTR>CELL idx FFI-STACK!
   len WRITABLE-LEN idx STACK-LEN! ;

: X8-VALUE! ( n -- ) 8 VALUE! ;
: X8-READABLE! ( ptr a -- ) 8 READABLE! ;
: X8-WRITABLE! ( ptr a n -- ) 8 WRITABLE! ;

: ARGS ( -- ptr n ) FFI-BUF ;
: FLOATS ( -- ptr r ) FFI-FBUF ;
: STACK ( -- ptr n ) FFI-STACK-BUF ;
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
: CSTR ( ptr u8 n ptr u8 -- ) COPY-CSTR ;

private

: ERRNO-SYMBOL$ ( -- ptr u8 n )
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if s" __errno_location" exit then
   HB-TARGET-MACOS? if s" __error" exit then
   E-FFI-LIBRARY throw ;

public

: DLOPEN ( ptr u8 n -- n ) DLOPEN-RAW ;
: DLSYM ( n ptr u8 -- n ) DLSYM-RAW ;

\ ---- the declared-function surface ----------------------------------------
\ DECLARE publishes one row and answers its index; the declarer below is its
\ only intended caller. PROCESS and LIBRARY-PATH answer the library index a
\ declaration resolves against.
: PROCESS ( -- n ) 0 ;

: LIBRARY-MAX ( -- n ) LIB-MAX ;

: LIBRARY-PATH-CAP ( -- n ) LIB-PATH-CAP ;

\ Render into the caller's CODEGEN buffer; the caller owns the returned span.
\ Independent callers can build names without sharing mutable scratch storage.
: LIBRARY-NAME$ ( ptr u8 n n ptr n -- ptr u8 n ) {: base:ptr u:n version:n buf:ptr :}
   u BASE-CHECK
   version VERSION-CHECK
   buf CODEGEN:RESET
   s" lib" buf CODEGEN:APPEND-STRING
   base u buf CODEGEN:APPEND-STRING
   HB-TARGET-MACOS? if
      s" ." buf CODEGEN:APPEND-STRING
      version buf CODEGEN:APPEND-DECIMAL
      s" .dylib" buf CODEGEN:APPEND-STRING
   else
      s" .so." buf CODEGEN:APPEND-STRING
      version buf CODEGEN:APPEND-DECIMAL
   then
   buf CODEGEN:CONTENTS ;

: LIBRARY-ROOM? ( -- bool ) LIB-N @ LIB-MAX < ;

\ The table's size and whether one more row is left. The declarer asks before it
\ registers, so a refusal can name the declaration package FFI never sees; the
\ ceiling is documented in docs/stdlib.md and read from here, never retyped.
: DECLARATION-MAX ( -- n ) FN-MAX ;

: ROOM? ( -- bool ) FN-N @ FN-MAX < ;

\ Path identity is the complete byte span, including its length. Reusing an
\ occupied row leaves its resolved handle in place, even when the table is full.
\ Only a new path consumes a row; the declarer catches that refusal so it can
\ name the pending FUNCTION: word and symbol.
: LIBRARY-PATH ( ptr u8 n -- n ) {: path:ptr u:n :}
   u PATH-ROOM drop
   LIB-N @ 0 ?do
      i LIB-LEN@ u = if
         path u i LIB-PATH u STR= if i 1+ unloop exit then
      then
   loop
   LIBRARY-ROOM? 0= if E-FFI-LIBRARY-FULL throw then
   path u LIB-N @ LIB-PATH CSTR
   u LIB-N @ LIB-LEN!
   0 LIB-N @ LIB-HANDLE!
   LIB-N @ 1 + dup LIB-N ! ;

private

\ Close the handle in library slot `slot`, answering whether it closed. dlclose
\ answers a C int, which fills only the low half of the return register, and
\ zero is success, so the low half alone decides. A refused close keeps the
\ handle for the next PREPARE to retry. This runs only inside the hook FFI has
\ registered, so the dlclose row is looked up without registering.
: CLOSE-SLOT ( n -- bool ) {: slot:n :}
   RESET slot LIB-HANDLE@ 0 VALUE!
   DLCLOSE-FN-CELL @ {: row:n :}
   row FN-LOOKUP {: fn:n :}
   FFI-BUF FFI-REG-LEN-BUF row FN-ARGC@ fn ffi-call-bounded
   $FFFFFFFF and 0 <> if false exit then
   0 slot LIB-HANDLE!
   true ;

\ Image capture is quiescent. Library handles are owned references and are
\ released here; function addresses are borrowed and only cleared, and the next
\ call after a restore re-resolves against the new process. This runs after
\ every one-shot cleanup (IMAGE-LIFECYCLE:PREPARE runs persistent hooks last, in
\ reverse order, and FFI registers first), so module cleanup that calls into a
\ library has finished before the library closes.
: FORGET-SYMBOLS ( -- )
   true
   LIB-N @ 0 ?do
      i LIB-HANDLE@ 0 <> if i CLOSE-SLOT and then
   loop {: closed:bool :}
   FN-MAX 0 ?do 0 i FN-ADDR! loop
   FFI-BUF BYTE-VIEW FFI-KPARAM-PBUF-OFF FFI-BUF-OFF - FORGET-STAGED
   FFI-DLBUF BYTE-VIEW FFI-KPARAM#-OFF FFI-DLBUF-OFF - FORGET-STAGED
   FFI-REG-LEN-BUF BYTE-VIEW FFI-SCRATCH-END FFI-REG-LEN-BUF-OFF - FORGET-STAGED
   closed 0= if E-FFI-DLCLOSE throw then ;

\ The registered flag is set only after REGISTER completes, so a throwing
\ registration leaves flag and registry consistent for a retry (forth.md's
\ IMAGE-LIFECYCLE:PREPARE rule).
: REGISTER-CLEANUP ( -- )
   FN-REGISTERED @ 0= if
      [: FORGET-SYMBOLS ;] IMAGE-LIFECYCLE:REGISTER-PERSISTENT
      1 FN-REGISTERED !
   then ;

\ The resolution every call makes. Its unresolved path arms FFI's cleanup before
\ the lookup acquires an address or a library handle, and the cached path pays
\ nothing. A stripped image needs this: it starts with an empty registry and the
\ flag clear (OWNED-CELLS) and replays no declaration, so its first resolution
\ registers, and FFI's hook is its first persistent one unless the program
\ registered one at run time before its first foreign call. Registering here
\ rather than in the CALL words puts this word after REGISTER-CLEANUP, and so
\ after the hook's own dlclose call, which looks its row up with FN-LOOKUP.
: FN-RESOLVE ( n -- n ) {: idx:n :}
   idx FN-ADDR@ dup 0 <> if exit then
   drop
   REGISTER-CLEANUP
   idx FN-LOOKUP ;

public

\ The staged call. The row's index is the only thing a caller names, so a
\ checked body cannot reach an address the declarations did not publish.
: CALL ( n -- n ) {: idx:n :}
   idx FN-RESOLVE {: fn:n :}
   FFI-BUF FFI-REG-LEN-BUF idx FN-ARGC@ fn ffi-call-bounded ;

\ A foreign address carries no length; only this package may mint the pointer,
\ and a caller reads it with the width the callee's contract fixes.
: CALL-PTR ( n -- ptr u8 ) CALL CELL>PTR ;

: CALL-ABI ( n -- n ) {: idx:n :}
   idx FN-RESOLVE {: fn:n :}
   FFI-BUF FFI-FBUF FFI-STACK-BUF FFI-REG-LEN-BUF FFI-STACK-LEN-BUF
   idx FN-SPILL@ fn ffi-call-abi-bounded ;

: CALL-ABI-R ( n -- r ) {: idx:n :}
   idx FN-RESOLVE {: fn:n :}
   FFI-BUF FFI-FBUF FFI-STACK-BUF FFI-REG-LEN-BUF FFI-STACK-LEN-BUF
   idx FN-SPILL@ fn ffi-call-abi-r-bounded ;

\ The table's own fail-closed guard. The declarer refuses first, with the word
\ and symbol in its diagnostic; this stands for every other caller and for the
\ errno row below, and it is what keeps a full table from being overrun.
\ Declaring registers too. A process that loads this file declares the errno row
\ at its load, before any module that calls a library can register a persistent
\ hook, so FFI's hook is the first and runs last whenever the first foreign call
\ comes, and a full image keeps the entry and the flag (tools/aot-build-core.f
\ registers PROC-MAPS:RELOAD, which calls Mach through this table, after it).
: DECLARE-SPILLED ( ptr u8 n n n n -- n ) {: name:ptr u:n lib:n argc:n spills:n :}
   ROOM? 0= if E-FFI-TABLE-FULL throw then
   lib LIB-N @ 1 + FFI-CHECK-INDEX
   argc FFI-MAX-ARGS FFI-CHECK-COUNT
   spills FFI-MAX-ARGS FFI-CHECK-COUNT
   spills FN-SPILLS FN-N @ cells + !
   name u NAME-ROOM FN-N @ FN-NAME CSTR
   lib FN-LIBS FN-N @ cells + !
   argc FN-ARGCS FN-N @ cells + !
   0 FN-N @ FN-ADDR!
   REGISTER-CLEANUP
   FN-N @ dup 1 + FN-N ! ;

: DECLARE ( ptr u8 n n n -- n ) 0 DECLARE-SPILLED ;

\ errno is this package's own binding, written by hand because the declarer is
\ defined after this package closes. libc's location is thread-local and the
\ value is a four-byte C int.
: ERRNO ( -- n )
   RESET ERRNO-FN CALL-PTR LE:U32@ ;

ERRNO-SYMBOL$ PROCESS 0 DECLARE ERRNO-FN-CELL !

\ FORGET-SYMBOLS closes the library handles through this row.
s" dlclose" PROCESS 1 DECLARE DLCLOSE-FN-CELL !

\ A stripped image whose host already includes FFI needs its declarations in
\ the image's DATA window. The names, paths and row metadata are the declarations
\ made while loading the application; foreign addresses belong to the new
\ process and must start unresolved. Keep the table private and hand its exact
\ storage to the linker through the two callbacks.
: OWNED-CELLS ( [ ptr u8 n -- ] [ ptr u8 n -- ] -- )
   {: carry fresh :}
   FN-NAMES FN-MAX FN-NAME-CAP * carry execute
   FN-LIBS FN-MAX cells carry execute
   FN-ARGCS FN-MAX cells carry execute
   FN-SPILLS FN-MAX cells carry execute
   LIB-PATHS LIB-MAX LIB-PATH-CAP * carry execute
   LIB-LENS LIB-MAX cells carry execute
   FN-N BYTE-VIEW CELL carry execute
   LIB-N BYTE-VIEW CELL carry execute
   ERRNO-FN-CELL BYTE-VIEW CELL carry execute
   DLCLOSE-FN-CELL BYTE-VIEW CELL carry execute
   FN-ADDRS FN-MAX cells fresh execute
   LIB-HANDLES LIB-MAX cells fresh execute
   FN-REGISTERED BYTE-VIEW CELL fresh execute ;

\ Both wordlists are protected here, after FFI's last definition: an engine
\ that loads this file after its seal (tools/build-fixpoint.f's capture host)
\ refuses any definition into a protected wordlist and exits 84.
private

get-current prot-wid-add

public

get-current prot-wid-add

;package

\ ---------------------------------------------------------------------------
\ FUNCTION: - declare a foreign function, get a checked word.
\
\     VERSIONED-LIBRARY z 1                   \ libz.so.1 here, libz.1.dylib on macOS
\     FUNCTION: CRC-CALL crc32 ( n ptr u8 n -- n ) ;FUNCTION
\     LIBRARY /usr/lib/libSystem.B.dylib      \ a literal, or PROCESS-SYMBOLS (RTLD_DEFAULT)
\     FUNCTION: SQRT-CALL sqrt ( r -- r ) ;FUNCTION
\     FUNCTION: LOCAL-CALL getsockname ( n ptr u8 ptr u8 -- i32 )
\        1 $10 WRITES-BYTES                   \ sockaddr_in
\        2 $04 WRITES-BYTES                   \ socklen_t
\     ;FUNCTION
\
\ The Habu word name and the C symbol are separate tokens. A symbol is often not
\ a legal Habu name (__errno_location), and a package's own verb is usually not
\ the C one: UDP4:BIND is the module's API, bind is libc's.
\
\ The declared effect becomes the generated word's effect - verbatim, except that
\ an `i32` result reads `n` - and decides every argument's staging: `n` (and any
\ token that widens to it) is a VALUE!, `r` is a FLOAT!, and `ptr u8` is a
\ READABLE! - read-only, extent 0 - unless a clause names it written:
\ `idx len WRITES-BYTES` for a fixed width, or `idx arg WRITES-ARG` when another
\ argument carries the length. A written pointer must state its extent because
\ the bounded call guards exactly the span it is given. The clauses are ordinary
\ words the interpreter runs between the two keywords, so their numbers are
\ literals and comments sit among them.
\ Convert a nominal at the call site: a declaration speaks the C function's own
\ types.
\
\ VERSIONED-LIBRARY, LIBRARY or PROCESS-SYMBOLS selects the library for the
\ declarations that follow. VERSIONED-LIBRARY takes a base name and a soname
\ version and renders the spelling THIS TARGET loads (FFI:LIBRARY-NAME$); a
\ soname is the usual case and neither spelling exists on the other system.
\ LIBRARY takes one literal, for an absolute path or a name that carries no
\ version, and refuses a literal spelled in the other target's convention.
\ THE SELECTION BELONGS TO THE SCOPE THAT STATES IT - the package
\ section, or the global scope, the declarations land in. There is no default:
\ a declaration with no selection in its own scope is E-FFI-LIBRARY, so a file
\ cannot inherit the library a previously loaded file happened to select and
\ resolve its symbols through that handle's dependency tree.
\
\ The result is the call's, at the width of the C prototype: `n` (or a widening
\ token) for a cell-wide integer - `long`, `size_t`, `ssize_t`, `off_t` or an
\ address kept as a number - `i32` for a C `int` (an enum, `pid_t`,
\ `kern_return_t`), `u32` for an `unsigned int` (`mach_port_t`), `r`, `ptr u8`
\ for a foreign-owned address, or nothing, which drops the machine return cell.
\ A 32-bit result fills only the low half of the return register and neither
\ AAPCS64 nor SysV defines the high half, so a C `int` declared `n` can read -1
\ as $FFFFFFFF. `i32` sign-extends the low half and reads `n` to the checker,
\ which has no signed 32-bit type; `u32` masks it and keeps its own type.
\
\ Integer arguments take x0.. and floats d0.. in declaration order (AAPCS64). A
\ declaration carrying a float rides the ABI call, which packs no spill here, so
\ it is refused past eight of either kind (E-FFI-ARITY); an integer-only
\ declaration rides ffi-call-bounded, which spills past x7 itself.
\
\ The symbol resolves once per process, at the FIRST CALL and never at the
\ declaration, so an absent symbol is E-FFI-DLSYM where the caller stands;
\ package FFI re-resolves after an image restore.
\
\ Every declaration in the image shares FFI's one table. A declaration that
\ finds it full is E-FFI-TABLE-FULL, and the refusal is the declarer's because
\ only here are the Habu word and the C symbol both in hand: the diagnostic
\ names them on stderr before the throw, so a full table is one line rather
\ than a bisection over the libraries a process happens to load.
\
\ THE GENERATED TEXT DEFINES WORDS AND LEAVES NOTHING ON THE STACK. It crosses
\ the loader's INCLUDE-EVALUATE, which is evaluate-closed: the text runs with its
\ floor at the declarer's depth, and a text that left cells would be refused
\ E-EVAL-RESIDUE. The declarer's obligation to that boundary is one complete
\ colon definition per declaration and no interpreted residue; lib/ffi-test.f
\ asserts DEPTH across a declaration.

package FFI-DECL

private

$800 constant GEN-CAP
$100 constant EFF-CAP
$40 constant TOK-CAP
$10 constant ARG-MAX
8 constant REG-MAX                        \ AAPCS64 integer and float register slots
10 constant DEC-BASE
16 constant HEX-BASE
$20 constant SP-C
$24 constant HEX-C                        \ '$' - the hex literal prefix
$0A constant LF-C                         \ the diagnostic's line terminator
2 constant DIAG-FD                        \ stderr

: DECIMAL-WIDTH ( n -- n )
   1 swap begin dup 10 >= while 10 / swap 1+ swap repeat drop ;

\ The library-full line is the longest: a PATH-CAP path, two TOK-CAP tokens,
\ the library row count, its literal wording and the line terminator.
PATH-CAP
s" ffi: library table full at " nip +
FFI:LIBRARY-MAX DECIMAL-WIDTH +
s"  rows: no library row for " nip +
s" ; no declaration row for " nip +
2 TOK-CAP * +
s"  (symbol " nip +
s" )" nip +
1 + constant DIAG-CAP

0 constant K-VALUE
1 constant K-POINTER
2 constant K-FLOAT

0 constant R-NONE
1 constant R-VALUE
2 constant R-FLOAT
3 constant R-POINTER
4 constant R-I32                          \ a C int: sign-extend the low half
5 constant R-U32                          \ a C unsigned int: mask the low half

0 constant C-BOUNDED
1 constant C-POINTER
2 constant C-ABI
3 constant C-ABI-R

-1 constant NO-EXT

FFI:LIBRARY-PATH-CAP 1 - CODEGEN:BUFFER LIB-NAME
GEN-CAP CODEGEN:BUFFER GEN
EFF-CAP CODEGEN:BUFFER EFF
DIAG-CAP CODEGEN:BUFFER DIAG
FFI:LIBRARY-PATH-CAP CODEGEN:BUFFER LIB-OVERFLOW

create NAME-BUF TOK-CAP allot
create SYM-BUF TOK-CAP allot
create TOK-BUF TOK-CAP allot
create BASE-BUF TOK-CAP allot
variable NAME-U
variable SYM-U
variable TOK-U
variable BASE-U

create ARG-KIND ARG-MAX cells allot
create ARG-REG ARG-MAX cells allot
create ARG-EXT ARG-MAX cells allot
create ARG-EXT-ARG ARG-MAX cells allot
variable ARG-N
variable INT-N
variable FLT-N
variable RES-KIND
variable CALL-KIND
variable VAR-START
variable STACK-N
variable CUR-LIB
variable OPEN                              \ a declaration is between FUNCTION: and ;FUNCTION
variable SCOPE-WID                         \ the wordlist LIBRARY/PROCESS-SYMBOLS was stated in
variable SCOPE-SET                         \ ... and whether one was stated at all
variable SCOPE-HOOK

\ ---- the declaration token stream -----------------------------------------
\ parse-name's span is transient, so every token is copied before the next one
\ is read and every comparison reads the copy.

: TOK! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if E-FFI-SYNTAX throw then
   u TOK-CAP > if E-FFI-SYNTAX throw then
   a TOK-BUF u BYTE-COPY
   u TOK-U ! ;

: TOK@ ( -- )
   parse-name TOK! ;

: TOK$ ( -- ptr u8 n )
   TOK-BUF TOK-U @ ;

: TOK-IS? ( ptr u8 n -- bool )
   TOK$ STR= ;

: NAME! ( -- )
   TOK$ {: a:ptr u:n :}
   a NAME-BUF u BYTE-COPY
   u NAME-U ! ;

: SYM! ( -- )
   TOK$ {: a:ptr u:n :}
   a SYM-BUF u BYTE-COPY
   u SYM-U ! ;

\ VERSIONED-LIBRARY's two tokens. The base name is copied out of the token
\ buffer because reading the version token overwrites it.
: BASE! ( -- )
   TOK$ {: a:ptr u:n :}
   a BASE-BUF u BYTE-COPY
   u BASE-U ! ;

: TOK>VERSION ( -- n )
   TOK$ STR-PARSE-POS MATCH option
      none OF E-FFI-SYNTAX throw ENDOF
      some OF ENDOF
   ;MATCH ;

\ ---- the declared effect ---------------------------------------------------

: ARG-KIND@ ( n -- n ) {: i:n :} ARG-KIND i cells + @ ;
: ARG-REG@ ( n -- n ) {: i:n :} ARG-REG i cells + @ ;
: ARG-EXT@ ( n -- n ) {: i:n :} ARG-EXT i cells + @ ;
: ARG-EXT-ARG@ ( n -- n ) {: i:n :} ARG-EXT-ARG i cells + @ ;
: ARG-EXT! ( n n -- ) {: v:n i:n :} v ARG-EXT i cells + ! ;
: ARG-EXT-ARG! ( n n -- ) {: v:n i:n :} v ARG-EXT-ARG i cells + ! ;

: EFF+ ( ptr u8 n -- )
   EFF CODEGEN:APPEND-STRING
   SP-C EFF CODEGEN:APPEND-BYTE ;

: TOK-KEEP ( -- )
   TOK$ EFF+ ;

: REG+ ( n -- ) {: kind:n :}
   ARG-N @ ARG-MAX >= if E-FFI-ARITY throw then
   kind ARG-KIND ARG-N @ cells + !
   NO-EXT ARG-N @ ARG-EXT!
   NO-EXT ARG-N @ ARG-EXT-ARG!
   kind K-FLOAT = if
      FLT-N @ ARG-REG ARG-N @ cells + !
      FLT-N @ 1 + FLT-N !
   else
      INT-N @ ARG-REG ARG-N @ cells + !
      INT-N @ 1 + INT-N !
   then
   ARG-N @ 1 + ARG-N ! ;

\ A pointer is `ptr u8`: a byte span with a length the declaration states. `ptr`
\ alone, or over any other pointee, has no extent this side can guard.
: PTR-TAIL ( -- )
   TOK@
   s" u8" TOK-IS? 0= if E-FFI-SYNTAX throw then
   TOK-KEEP ;

: IN-TOKEN ( -- )
   s" ptr" TOK-IS? if TOK-KEEP PTR-TAIL K-POINTER REG+ exit then
   s" r" TOK-IS? if TOK-KEEP K-FLOAT REG+ exit then
   TOK-KEEP K-VALUE REG+ ;

\ `i32` is not a checker type: the word the declaration generates returns the
\ sign-extended value as `n`.
: OUT-TOKEN ( -- )
   RES-KIND @ R-NONE <> if E-FFI-SYNTAX throw then
   s" ptr" TOK-IS? if TOK-KEEP PTR-TAIL R-POINTER RES-KIND ! exit then
   s" r" TOK-IS? if TOK-KEEP R-FLOAT RES-KIND ! exit then
   s" i32" TOK-IS? if s" n" EFF+ R-I32 RES-KIND ! exit then
   s" u32" TOK-IS? if TOK-KEEP R-U32 RES-KIND ! exit then
   TOK-KEEP R-VALUE RES-KIND ! ;

: INPUTS ( -- )
   begin
      TOK@
      s" --" TOK-IS? if TOK-KEEP exit then
      s" )" TOK-IS? if E-FFI-SYNTAX throw then
      IN-TOKEN
   again ;

: OUTPUTS ( -- )
   begin
      TOK@
      s" )" TOK-IS? if exit then
      s" --" TOK-IS? if E-FFI-SYNTAX throw then
      OUT-TOKEN
   again ;

: EFFECT ( -- )
   TOK@
   s" (" TOK-IS? 0= if E-FFI-SYNTAX throw then
   EFF CODEGEN:RESET
   INPUTS
   OUTPUTS ;

\ ---- which call the declaration rides --------------------------------------

: REG-ROOM ( -- )
   INT-N @ REG-MAX > if E-FFI-ARITY throw then
   FLT-N @ REG-MAX > if E-FFI-ARITY throw then ;

: PLAN-SPILLS ( -- )
   0 STACK-N !
   VAR-START @ 0 < HB-TARGET-MACOS? 0= or if exit then
   ARG-N @ VAR-START @ - STACK-N !
   0 INT-N ! 0 FLT-N !
   VAR-START @ 0 ?do
      i ARG-KIND@ K-FLOAT = if 1 FLT-N +! else 1 INT-N +! then
   loop
   REG-ROOM ;

: PLAN-CALL ( -- )
   PLAN-SPILLS
   STACK-N @ 0 > if
      RES-KIND @ R-POINTER = if E-FFI-ARITY throw then
      RES-KIND @ R-FLOAT = if C-ABI-R else C-ABI then CALL-KIND ! exit
   then
   RES-KIND @ R-FLOAT = if REG-ROOM C-ABI-R CALL-KIND ! exit then
   FLT-N @ 0 > if
      RES-KIND @ R-POINTER = if E-FFI-ARITY throw then
      REG-ROOM C-ABI CALL-KIND ! exit
   then
   RES-KIND @ R-POINTER = if C-POINTER CALL-KIND ! exit then
   C-BOUNDED CALL-KIND ! ;

\ ---- the generated definition ----------------------------------------------

: GEN+ ( ptr u8 n -- ) GEN CODEGEN:APPEND-STRING ;
: GEN-N ( n -- ) GEN CODEGEN:APPEND-DECIMAL ;
: GEN-SP ( -- ) SP-C GEN CODEGEN:APPEND-BYTE ;
: GEN-ARG ( n -- ) s" A" GEN+ GEN-N ;

: LOCALS ( -- )
   ARG-N @ 0= if exit then
   s" {: " GEN+
   ARG-N @ 0 ?do
      i GEN-ARG
      i ARG-KIND@ K-VALUE = if s" :n" GEN+ then
      i ARG-KIND@ K-FLOAT = if s" :r" GEN+ then
      GEN-SP
   loop
   s" :} " GEN+ ;

: STAGE-VALUE ( n -- ) {: i:n :}
   i GEN-ARG GEN-SP i ARG-REG@ GEN-N s"  FFI:VALUE! " GEN+ ;

: STAGE-FLOAT ( n -- ) {: i:n :}
   i GEN-ARG GEN-SP i ARG-REG@ GEN-N s"  FFI:FLOAT! " GEN+ ;

: STAGE-READABLE ( n -- ) {: i:n :}
   i GEN-ARG GEN-SP i ARG-REG@ GEN-N s"  FFI:READABLE! " GEN+ ;

: STAGE-WRITABLE ( n -- ) {: i:n :}
   i GEN-ARG GEN-SP
   i ARG-EXT@ NO-EXT <> if i ARG-EXT@ GEN-N else i ARG-EXT-ARG@ GEN-ARG then
   GEN-SP i ARG-REG@ GEN-N s"  FFI:WRITABLE! " GEN+ ;

: STAGE-POINTER ( n -- ) {: i:n :}
   i ARG-EXT@ NO-EXT <> if i STAGE-WRITABLE exit then
   i ARG-EXT-ARG@ NO-EXT <> if i STAGE-WRITABLE exit then
   i STAGE-READABLE ;

: STACK-POS ( n -- ) VAR-START @ - GEN-N ;
: STAGE-SPILL ( n -- ) {: i:n :}
   i GEN-ARG GEN-SP
   i ARG-KIND@ K-FLOAT = if
      i STACK-POS s"  FFI:STACK-FLOAT! " GEN+ exit then
   i ARG-KIND@ K-VALUE = if
      i STACK-POS s"  FFI:STACK-VALUE! " GEN+ exit then
   i ARG-EXT@ NO-EXT <> i ARG-EXT-ARG@ NO-EXT <> or if
      i ARG-EXT@ NO-EXT <> if i ARG-EXT@ GEN-N else i ARG-EXT-ARG@ GEN-ARG then
      GEN-SP i STACK-POS s"  FFI:STACK-WRITABLE! " GEN+ exit
   then
   i STACK-POS s"  FFI:STACK-READABLE! " GEN+ ;

: STAGE-ONE ( n -- ) {: i:n :}
   STACK-N @ 0 > i VAR-START @ >= and if i STAGE-SPILL exit then
   i ARG-KIND@ K-FLOAT = if i STAGE-FLOAT exit then
   i ARG-KIND@ K-VALUE = if i STAGE-VALUE exit then
   i STAGE-POINTER ;

: STAGE ( -- )
   ARG-N @ 0 ?do i STAGE-ONE loop ;

: CALL-WORD ( -- )
   CALL-KIND @ C-ABI-R = if s" FFI:CALL-ABI-R" GEN+ exit then
   CALL-KIND @ C-ABI = if s" FFI:CALL-ABI" GEN+ exit then
   CALL-KIND @ C-POINTER = if s" FFI:CALL-PTR" GEN+ exit then
   s" FFI:CALL" GEN+ ;

\ ---- the row this declaration takes ----------------------------------------
\ Package FFI owns the table and refuses a full one itself. The declarer asks
\ first because the two facts a refusal has to carry - the Habu word being
\ defined and the C symbol behind it - are held here and nowhere else. The
\ line goes to stderr exactly as a load-time diagnostic does, and the throw
\ that follows is what ends the load.

: DIAG-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   DIAG-FD a u write drop ;

: DIAG+ ( ptr u8 n -- )
   DIAG CODEGEN:APPEND-STRING ;

: REPORT-LIBRARY-FULL ( -- )
   DIAG CODEGEN:RESET
   s" ffi: library table full at " DIAG+
   FFI:LIBRARY-MAX DIAG CODEGEN:APPEND-DECIMAL
   s"  rows: no library row for " DIAG+
   LIB-OVERFLOW CODEGEN:CONTENTS DIAG+
   s" ; no declaration row for " DIAG+
   NAME-BUF NAME-U @ DIAG+
   s"  (symbol " DIAG+
   SYM-BUF SYM-U @ DIAG+
   s" )" DIAG+
   LF-C DIAG CODEGEN:APPEND-BYTE
   DIAG CODEGEN:CONTENTS DIAG-LINE ;

\ A library name spelled in the OTHER target's convention cannot resolve on this
\ one, and dlopen would only say so at the first call, in whatever module the
\ declaration belongs to. It is refused where it is stated instead, and the line
\ names the path and the target this build loads for, the way the table-full
\ line names its path. A rendered name passes by construction, so the guard
\ costs a literal row nothing.
: TARGET$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if s" macos" exit then s" linux" ;

: BASENAME ( ptr u8 n -- ptr u8 n ) {: path:ptr u:n :}
   0 u 0 ?do path i + c@ [char] / = if drop i 1+ then loop
   {: start:n :} path start + u start - ;

: FOREIGN-SPELLING? ( ptr u8 n -- bool )
   BASENAME {: path:ptr u:n :}
   HB-TARGET-MACOS? if
      path u s" .dylib" ENDS-WITH? if false exit then
      path u s" .so" ENDS-WITH? path u s" .so." CONTAINS? or exit
   then
   path u s" .dylib" ENDS-WITH? ;

: REPORT-LIBRARY-TARGET ( ptr u8 n -- ) {: path:ptr u:n :}
   DIAG CODEGEN:RESET
   s" ffi: library " DIAG+
   path u DIAG+
   s"  is spelled for the other target; this build loads for " DIAG+
   TARGET$ DIAG+
   s" : name it with VERSIONED-LIBRARY" DIAG+
   LF-C DIAG CODEGEN:APPEND-BYTE
   DIAG CODEGEN:CONTENTS DIAG-LINE ;

: CHECK-SPELLING ( ptr u8 n -- ) {: path:ptr u:n :}
   path u FOREIGN-SPELLING? 0= if exit then
   path u REPORT-LIBRARY-TARGET
   E-FFI-LIBRARY throw ;

: REPORT-FULL ( -- )
   DIAG CODEGEN:RESET
   s" ffi: declaration table full at " DIAG+
   FFI:DECLARATION-MAX DIAG CODEGEN:APPEND-DECIMAL
   s"  rows: no row for " DIAG+
   NAME-BUF NAME-U @ DIAG+
   s"  (symbol " DIAG+
   SYM-BUF SYM-U @ DIAG+
   s" )" DIAG+
   LF-C DIAG CODEGEN:APPEND-BYTE
   DIAG CODEGEN:CONTENTS DIAG-LINE ;

: TABLE-ROOM ( -- )
   FFI:ROOM? if exit then
   REPORT-FULL
   E-FFI-TABLE-FULL throw ;

: SLOT ( -- n )
   CUR-LIB @ -1 = if
      REPORT-LIBRARY-FULL
      E-FFI-LIBRARY-FULL throw
   then
   TABLE-ROOM
   SYM-BUF SYM-U @ CUR-LIB @ INT-N @ STACK-N @ FFI:DECLARE-SPILLED ;

\ What the machine return cell becomes: dropped, rebuilt from its low half for a
\ 32-bit result, or taken whole.
: RESULT-TAIL ( -- )
   RES-KIND @ R-NONE = if s"  drop" GEN+ exit then
   RES-KIND @ R-I32 = if
      s"  $FFFFFFFF and dup $80000000 and 0 <> if $FFFFFFFF00000000 or then" GEN+
      exit
   then
   RES-KIND @ R-U32 = if s"  $FFFFFFFF and" GEN+ then ;

: EMIT ( -- )
   GEN CODEGEN:RESET
   s" : " GEN+ NAME-BUF NAME-U @ GEN+
   s"  ( " GEN+ EFF CODEGEN:CONTENTS GEN+ s" ) " GEN+
   LOCALS
   s" FFI:RESET " GEN+
   STAGE
   SLOT GEN-N GEN-SP
   CALL-WORD
   RESULT-TAIL
   s"  ;" GEN+ ;

\ A selection belongs to the scope that states it - the package section, or the
\ global scope, that will hold the declarations - and there is no default. The
\ scope is the current WORDLIST rather than the loading file because the loader
\ has no current-file identity to ask for: INCLUDE-PATH names the last file
\ OPENED, so after `require lib/ffi-abi.f` it reads lib/codegen.f, and every FFI
\ consumer would share that one identity. A wordlist is the module a definition
\ lands in, it is saved and restored with the package scope, and it separates two
\ consumers even when their requires end at the same file.
\ A selected declaration scope belongs to the source load that made it. The
\ source is closed before capture, and its host wordlist id is not an image id.
: CLEAR-SCOPE ( -- )
   0 SCOPE-WID !
   0 SCOPE-SET ! ;

: RECORD-SCOPE ( -- )
   SCOPE-HOOK @ 0= if
      [: CLEAR-SCOPE ;] IMAGE-LIFECYCLE:REGISTER-PERSISTENT
      1 SCOPE-HOOK !
   then
   get-current SCOPE-WID !
   1 SCOPE-SET ! ;

: TAKE-LIBRARY ( -- )
   LIB-OVERFLOW CODEGEN:CONTENTS FFI:LIBRARY-PATH CUR-LIB ! ;

: CHECK-SCOPE ( -- )
   SCOPE-SET @ 0= if E-FFI-LIBRARY throw then
   get-current SCOPE-WID @ <> if E-FFI-LIBRARY throw then ;

public

\ FUNCTION: parses the name, the symbol and the effect and leaves the
\ declaration open; the interpreter then runs any clauses; ;FUNCTION plans it,
\ renders it and hands the complete definition to the loader's closed
\ evaluate. Nothing is published before the closer, and the row is registered as
\ the text is rendered, so a refused declaration leaves no half-built word.
: OPEN-FUNCTION ( -- )
   CHECK-SCOPE
   OPEN @ 0 <> if E-FFI-SYNTAX throw then
   0 ARG-N ! 0 INT-N ! 0 FLT-N ! -1 VAR-START ! 0 STACK-N !
   R-NONE RES-KIND !
   TOK@ NAME!
   TOK@ SYM!
   EFFECT
   1 OPEN ! ;

: CLOSE-FUNCTION ( -- )
   OPEN @ 0= if E-FFI-SYNTAX throw then
   0 OPEN !
   PLAN-CALL
   EMIT
   GEN CODEGEN:CONTENTS INCLUDE-EVALUATE ;

\ A refused clause ABANDONS the declaration rather than leaving it half open:
\ the next FUNCTION: in the file would otherwise refuse too, and the first
\ diagnostic would name the wrong declaration.
: REFUSE ( -- )
   0 OPEN !
   E-FFI-SYNTAX throw ;

: CLAUSE-TARGET ( n -- ) {: idx:n :}
   OPEN @ 0= if REFUSE then
   idx 0 < if REFUSE then
   idx ARG-N @ >= if REFUSE then
   idx ARG-KIND@ K-POINTER <> if REFUSE then
   idx ARG-EXT@ NO-EXT <> if REFUSE then
   idx ARG-EXT-ARG@ NO-EXT <> if REFUSE then ;

: SET-VARIADIC ( n -- ) {: fixed:n :}
   OPEN @ 0= if REFUSE then
   VAR-START @ -1 <> if REFUSE then
   fixed 1 < fixed ARG-N @ > or if REFUSE then
   fixed VAR-START ! ;

\ Argument idx is written by the callee: the bounded call guards exactly the
\ span named here, a fixed width or the argument that carries the length.
: WRITE-EXTENT ( n n -- ) {: idx:n len:n :}
   idx CLAUSE-TARGET
   len 0 <= if REFUSE then
   len idx ARG-EXT! ;

: WRITE-LENGTH-ARG ( n n -- ) {: idx:n src:n :}
   idx CLAUSE-TARGET
   src 0 < if REFUSE then
   src ARG-N @ >= if REFUSE then
   src ARG-KIND@ K-VALUE <> if REFUSE then
   src idx ARG-EXT-ARG! ;

: SELECT-LIBRARY ( ptr u8 n -- ) {: path:ptr u:n :}
   \ LIB-OVERFLOW holds CAP bytes; the length is bounded before the spelling check
   \ reads the path.
   u 0 <= u FFI:LIBRARY-PATH-CAP >= or if E-FFI-SYNTAX throw then
   path u CHECK-SPELLING
   LIB-OVERFLOW CODEGEN:RESET
   path u LIB-OVERFLOW CODEGEN:APPEND-STRING
   [: TAKE-LIBRARY ;] catch {: code:n :}
   code 0 <> if
      code E-FFI-LIBRARY-FULL <> if code throw then
      -1 CUR-LIB !
   then
   RECORD-SCOPE ;

\ The versioned form's own selection: two tokens, rendered for this target and
\ then taken through the literal form's path, so the scope recording and the
\ table guard are shared and a rendered name reaches dlopen the way a literal
\ one does.
: SELECT-VERSIONED-LIBRARY ( -- )
   TOK@ BASE!
   TOK@ TOK>VERSION {: version:n :}
   BASE-BUF BASE-U @ version LIB-NAME FFI:LIBRARY-NAME$ SELECT-LIBRARY ;

: SELECT-PROCESS ( -- )
   FFI:PROCESS CUR-LIB !
   RECORD-SCOPE ;

;package

\ The declaration keywords are global: a package word ending in a colon cannot
\ be reached through a qualifier, and a declarer reads bare from any package
\ anyway, like DEFTYPE and CAST:. All three parse the live input stream, which
\ their ( -- ) rows do not model, so they are top-level-interpret-only.
: LIBRARY ( -- )
   parse-name FFI-DECL:SELECT-LIBRARY ;

: VERSIONED-LIBRARY ( -- )
   FFI-DECL:SELECT-VERSIONED-LIBRARY ;

: PROCESS-SYMBOLS ( -- )
   FFI-DECL:SELECT-PROCESS ;

: FUNCTION: ( -- )
   FFI-DECL:OPEN-FUNCTION ;

: WRITES-BYTES ( n n -- )
   FFI-DECL:WRITE-EXTENT ;

: WRITES-ARG ( n n -- )
   FFI-DECL:WRITE-LENGTH-ARG ;

: VARIADIC ( n -- ) FFI-DECL:SET-VARIADIC ;

: ;FUNCTION ( -- )
   FFI-DECL:CLOSE-FUNCTION ;
