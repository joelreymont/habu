\ Persistent application state exercises DATA and compiled-code relocation.
package APP-IMAGE-SUBJECT

variable VALUE
PERSISTED-PTR-VARIABLE LINK
TYPED-VARIABLE ACTION [ n -- n ]

: INCREMENT ( n -- n ) 1+ ;
: DOUBLE ( n -- n ) 2 * ;
: PUT ( [ a -- a ] ptr [ a -- a ] -- ) ! ;

: INITIALIZE ( -- )
   42 VALUE !
   VALUE LINK !
   [: DOUBLE ;] ACTION PUT
   [: INCREMENT ;] ACTION PUT ;

INITIALIZE

public

: RUN ( -- n )
   LINK @ @ ACTION @ execute ;

;package
