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

: INCREMENT ( n -- n ) 1+ ;
: DOUBLE ( n -- n ) 2 * ;
: PUT ( [ a -- a ] ptr [ a -- a ] -- ) ! ;

: INITIALIZE ( -- )
   CHECK-UNICODE
   42 VALUE !
   VALUE LINK !
   [: DOUBLE ;] ACTION PUT
   [: INCREMENT ;] ACTION PUT ;

INITIALIZE

public

: RUN ( -- n )
   CHECK-UNICODE
   LINK @ @ ACTION @ execute ;

;package
