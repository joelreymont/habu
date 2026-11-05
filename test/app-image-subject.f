\ Persistent application state exercises DATA and compiled-code relocation.
require src/lib/string.f
require src/lib/unicode.f
require src/lib/image-lifecycle.f
require src/core/include.f

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
variable ORIGIN-N

: CHECK-ORIGIN ( n -- )
   ENGINE-ORIGIN:TARGET >r 2drop 2drop 2drop r>
   0= if 70 throw then
   1 ORIGIN-N +! ;

\ Cleanup can inspect source locations. A query here must not leave a
\ process-local lookup mapping in the executable saved immediately afterward.
: CHECK-ORIGINS ( -- )
   0 ORIGIN-N !
   s" src/lib/string.f" ['] CHECK-ORIGIN ENGINE-ORIGIN:EACH-IN
   ORIGIN-N @ 0= if 70 throw then ;

: IGNORE-SOURCE ( ptr u8 n ptr u8 n ptr u8 n -- )
   2drop 2drop 2drop ;

: USE-SCRATCH ( -- )
   1 SCRATCH-RESERVE
   23 0 SCRATCH !
   0 SCRATCH @ 23 <> if 70 throw then ;

: INCREMENT ( n -- n ) 1+ ;
: DOUBLE ( n -- n ) 2 * ;
: PUT ( [ a -- a ] ptr [ a -- a ] -- ) ! ;

: INITIALIZE ( -- )
   USE-SCRATCH
   CHECK-UNICODE
   ['] CHECK-ORIGINS IMAGE-LIFECYCLE:REGISTER
   ['] IGNORE-SOURCE ['] CHECK-ORIGINS SOURCE-INPUT:USE-OBSERVER
   42 VALUE !
   VALUE LINK !
   [: DOUBLE ;] ACTION PUT
   [: INCREMENT ;] ACTION PUT ;

INITIALIZE

public

: RUN ( -- n )
   CHECK-ORIGINS
   USE-SCRATCH
   CHECK-UNICODE
   LINK @ @ ACTION @ execute ;

;package
