\ Publish an emission into the engine code space and execute its bytes.
\ It runs only inside the window test/mcode-window-prepare.f opens: publication
\ appends through NPUB:WRITE-AT and C ABI entries use FFI:CALL-AT, the public
\ words that window gives the code arena's and the FFI's owner rows. Habu ABI
\ entries refine the emitted address to the quotation effect promised by the
\ fixture's source program; that test-only machine-code boundary does not
\ certify arbitrary addresses. Publication advances the code pointer past the
\ bytes, so each published routine keeps its slot for the call that follows and
\ the next publication lands after it.
\ Store at source-map offsets so execution also checks the emitted layout.

require lib/le.f
require lib/ffi-abi.f
require src/compiler/native/emit.f

package NRUN
using A64EMIT

private

\ The most bytes one published routine holds: 64 instructions, more than any
\ executing case emits.
$100 constant CODE-CAP
CODE-CAP BUFFER: CODE


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

\ Lay the sealed emission out little-endian in CODE, append it at the free code
\ slot and answer its entry address. It must be called from inside a definition:
\ a top-level publication would write over the line being interpreted.
: PUBLISH ( -- n )
   cp@ {: fn:n :}
   MAP-CHECK fn PLACE-CHECK fn ROOM-CHECK
   SIZE CODE-CAP > if E-NPUB-SIZE throw then
   INSNS 0 ?do
      i WORD@ CODE i MAP-OFFSET@ + LE:U32!
   loop
   CODE fn SIZE NPUB:WRITE-AT
   fn ;

;using

: EXEC0 ( n -- n ) {: fn:n :}
   FFI:RESET
   0 fn FFI:CALL-AT ;

: EXEC1 ( n n -- n ) {: a:n fn:n :}
   FFI:RESET
   a 0 FFI:VALUE!
   1 fn FFI:CALL-AT ;

: EXEC2 ( n n n -- n ) {: a:n b:n fn:n :}
   FFI:RESET
   a 0 FFI:VALUE!
   b 1 FFI:VALUE!
   2 fn FFI:CALL-AT ;

: EXEC3 ( n n n n -- n ) {: a:n b:n c:n fn:n :}
   FFI:RESET
   a 0 FFI:VALUE!
   b 1 FFI:VALUE!
   c 2 FFI:VALUE!
   3 fn FFI:CALL-AT ;

private

\ These are the raw machine-code entry refinements. The wrappers that invoke
\ them are checked; only the claimed effect of externally emitted bytes is
\ outside the source checker, as it is for a foreign function.
CAST: XT0 ( n -- [ -- ] )
CAST: XT1 ( n -- [ n -- n ] )
CAST: XT2 ( n -- [ n n -- n ] )
CAST: XT3 ( n -- [ n n n -- n ] )
CAST: XT-SPAN ( n -- [ ptr u8 n -- n ] )
CAST: XT-SPAN1 ( n -- [ ptr u8 n n -- n ] )

public

: ENTER0 ( n -- ) XT0 execute ;
: ENTER1 ( n n -- n ) XT1 execute ;
: ENTER2 ( n n n -- n ) XT2 execute ;
: ENTER3 ( n n n n -- n ) XT3 execute ;
: ENTER-SPAN ( ptr u8 n n -- n ) XT-SPAN execute ;
: ENTER-SPAN1 ( ptr u8 n n n -- n ) XT-SPAN1 execute ;

;package
