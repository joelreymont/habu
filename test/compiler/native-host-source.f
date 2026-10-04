\ Checked source loaded by the native build's source-bind callback, before
\ native-runtime.f. The host and target bodies are distinct publications.

package NATIVE-HOST-SOURCE
private

variable WRITES
CAST: >BYTES ( n -- ptr u8 )

: STEP ( n -- n ) abs ;
: NEXT ( n -- n ) STEP ;
: WRITE-ONCE ( -- ) 1 WRITES ! ;
: BAD-STORE ( -- n ) WRITE-ONCE 8 ;
: BAD-COMMA ( -- n ) 17 , 8 ;
: BAD-BYTE ( -- n ) 1 WRITES BYTE-VIEW c! 8 ;
: BAD-CAST ( -- n ) 42 >BYTES drop 8 ;
: BAD-POINTER ( -- n ) WRITES BYTE-VIEW drop 8 ;
: BAD-NATIVE ( -- n ) -7 negate ;
: BAD-DIVIDE ( n -- n ) 7 swap / ;
: BAD-EXEC ( -- n ) 1 0= if [: 7 ;] execute drop then 9 ;
: BAD-CATCH ( -- n ) [: ;] catch drop 8 ;
: BAD-FINALLY ( -- n ) [: 8 ;] [: ;] finally ;
: CALLBACK ( [ -- n ] -- n ) execute ;
: BAD-CALLBACK ( -- n ) [: 8 ;] CALLBACK ;
TRUSTED: BAD-TRUST ( -- n ) 8 ;

defer LATE ( -- n )
[: 7 ;] is LATE
: BAD-DEFER ( -- n ) LATE ;

public

: HOST42 ( -- n ) -42 NEXT ;
: TARGET99 ( -- n ) 99 ;
: BOOL-TARGET ( -- bool ) 0 0= ;
: LARGE-SCALAR ( -- n ) $7FFF000000001234 ;
: BEFORE-BAD ( -- n ) WRITE-ONCE BAD-NATIVE ;
: NESTED-STORE ( -- n ) BAD-STORE ;
: DIRECT-NATIVE ( -- n ) BAD-NATIVE ;
: DIVIDE-COLD ( -- n ) 1 BAD-DIVIDE ;
: CAST-BODY ( -- n ) BAD-CAST ;
: COMMA-BODY ( -- n ) BAD-COMMA ;
: BYTE-BODY ( -- n ) BAD-BYTE ;
: POINTER-BODY ( -- n ) BAD-POINTER ;
: INDIRECT ( -- n ) BAD-EXEC ;
: CAUGHT ( -- n ) BAD-CATCH ;
: FINALIZED ( -- n ) BAD-FINALLY ;
: DEFERRED ( -- n ) BAD-DEFER ;
: CALLBACK-ROOT ( -- n ) BAD-CALLBACK ;
: TRUSTED-ROOT ( -- n ) BAD-TRUST ;
: OLD-CALLER ( -- n ) HOST42 ;

: WRITES@ ( -- n ) WRITES @ ;
: WRITES-CLEAR ( -- ) 0 WRITES ! ;

;package
