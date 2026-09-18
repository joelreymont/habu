\ Publish an emission into the engine code space and execute its bytes.
\ C ABI entries use the bounded FFI call. Habu ABI entries refine the emitted
\ address to the quotation effect promised by the fixture's source program;
\ that test-only machine-code boundary does not certify arbitrary addresses.
\ The bytes run immediately in the free slot; no snapshot retains this slot.
\ Store at source-map offsets so execution also checks the emitted layout.

require lib/ffi-abi.f
require src/compiler/native/emit.f

package NRUN
using A64EMIT

private

TRUSTED: POKE ( n n -- ) patch32 ;


: MAP-CHECK ( -- )
   INSNS 0 ?do
      i MAP-OFFSET@ i 4 * <> if E-NPUB-OFFSET throw then
   loop ;


: PLACE-CHECK ( n -- ) {: fn:n :}
   PLACED? if
      PLACEMENT fn <> if E-NPUB-PLACE throw then
   then ;


: ROOM-CHECK ( n -- ) {: fn:n :}
   INSNS 0 <= if E-NPUB-SIZE throw then
   dbase@ REGION + $4000 - {: ceiling:n :}
   fn ceiling > if E-NPUB-ROOM throw then
   SIZE ceiling fn - > if E-NPUB-ROOM throw then ;

public

\ Declare the slot this emission will be published into BEFORE it is laid out. A
\ form whose branch leaves the emission - the division's refusal, the terminator
\ that ends the process - measures its displacement from where the bytes land, so
\ it is refused outright without a placement. The slot named is the one PUBLISH
\ below claims, so the displacement an executed case branches on is the real one.
: PLACE ( -- )
   cp@ PLACE-AT ;

\ Store the sealed emission into the free code slot and answer its entry address.
\ It must be called from inside a definition: a top-level `cp@` patch overwrites
\ the line being interpreted.
: PUBLISH ( -- n )
   cp@ {: fn:n :}
   MAP-CHECK fn PLACE-CHECK fn ROOM-CHECK
   INSNS 0 ?do
      i WORD@ fn i MAP-OFFSET@ + POKE
   loop
   fn ;

;using

TRUSTED: EXEC0 ( n -- n ) {: fn:n :}
   FFI:RESET
   FFI:ARGS FFI:REG-LENS 0 fn ffi-call-bounded ;

TRUSTED: EXEC1 ( n n -- n ) {: a:n fn:n :}
   FFI:RESET
   a 0 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 1 fn ffi-call-bounded ;

TRUSTED: EXEC2 ( n n n -- n ) {: a:n b:n fn:n :}
   FFI:RESET
   a 0 FFI:VALUE!
   b 1 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 2 fn ffi-call-bounded ;

TRUSTED: EXEC3 ( n n n n -- n ) {: a:n b:n c:n fn:n :}
   FFI:RESET
   a 0 FFI:VALUE!
   b 1 FFI:VALUE!
   c 2 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 3 fn ffi-call-bounded ;

private

\ These are the raw machine-code entry refinements. The wrappers that invoke
\ them are checked; only the claimed effect of externally emitted bytes is
\ outside the source checker, as it is for a foreign function.
TRUSTED: XT0 ( n -- [ -- ] ) ;
TRUSTED: XT1 ( n -- [ n -- n ] ) ;
TRUSTED: XT2 ( n -- [ n n -- n ] ) ;
TRUSTED: XT3 ( n -- [ n n n -- n ] ) ;
TRUSTED: XT-SPAN ( n -- [ ptr u8 n -- n ] ) ;
TRUSTED: XT-SPAN1 ( n -- [ ptr u8 n n -- n ] ) ;

public

: ENTER0 ( n -- ) XT0 execute ;
: ENTER1 ( n n -- n ) XT1 execute ;
: ENTER2 ( n n n -- n ) XT2 execute ;
: ENTER3 ( n n n n -- n ) XT3 execute ;
: ENTER-SPAN ( ptr u8 n n -- n ) XT-SPAN execute ;
: ENTER-SPAN1 ( ptr u8 n n n -- n ) XT-SPAN1 execute ;

;package
