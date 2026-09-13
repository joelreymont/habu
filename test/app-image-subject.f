\ Persistent application state exercises DATA and compiled-code relocation.
require lib/unicode.f

package APP-IMAGE-SUBJECT

create FOLDED 32 allot

: CHECK-UNICODE ( -- )
   s" Straße" s" STRASSE" UNICODE:CASEFOLD= 0= if 70 throw then
   s" İ" FOLDED 32 UNICODE:FOLD
   FOLDED swap s" i̇" STR= 0= if 70 throw then ;

variable VALUE
PERSISTED-PTR-VARIABLE LINK
TYPED-VARIABLE ACTION [ n -- n ]
DYNAMIC-BUFFER SCRATCH n

: USE-SCRATCH ( -- )
   1 SCRATCH-RESERVE
   23 0 SCRATCH !
   0 SCRATCH @ 23 <> if 70 throw then ;

: SCRATCH-ADDRESS ( -- ) 0 SCRATCH drop ;

: INCREMENT ( n -- n ) 1+ ;
: DOUBLE ( n -- n ) 2 * ;
: PUT ( [ a -- a ] ptr [ a -- a ] -- ) ! ;

: INITIALIZE ( -- )
   USE-SCRATCH
   CHECK-UNICODE
   42 VALUE !
   VALUE LINK !
   [: DOUBLE ;] ACTION PUT
   [: INCREMENT ;] ACTION PUT ;

INITIALIZE

public

\ Inspect bounds without dereferencing a stale process pointer. Both restored
\ generations must start released even though this declaration is never replayed.
: SCRATCH-CLEAN ( -- )
   [: SCRATCH-ADDRESS ;] catch 7122 <> if 70 throw then ;

: RUN ( -- n )
   USE-SCRATCH
   CHECK-UNICODE
   LINK @ @ ACTION @ execute ;

;package
