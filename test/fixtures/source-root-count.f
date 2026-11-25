split-str
\ source-root-count.f - the counts test/source-root-test.f's fixtures and the
\ files test/source-root-alias-child.f loads record as they load; the tests
\ read them. A fixture requires this file by its absolute path, which names
\ this one file whatever root the fixture is read from.

package SOURCE-ROOT-COUNT
public
variable LOADED              \ the sum of BUMP's arguments
variable ACTIVE              \ frames ENTER opened, LEAVE-FRAME has not closed
variable VISITS              \ ENTER's calls
variable WRONG               \ calls out of order or under a root not ROOT!'s
private

create ROOT PATH-CAP allot
variable ROOT-U

: AT-ROOT ( -- )
   SOURCE-ROOT:CURRENT$ ROOT ROOT-U @ STR:= 0= if 1 WRONG +! then ;

public
: ROOT! ( str -- ) {: a:^ u:n :}
   u PATH-CAP > if E-PATH-RANGE throw then
   a ROOT u BYTE-COPY u ROOT-U ! ;

: BUMP ( n -- ) LOADED +! ;

\ Frame n opens while n frames are open, and closes leaving n open.
: ENTER ( n -- )
   ACTIVE @ <> if 1 WRONG +! then
   1 ACTIVE +! 1 VISITS +! AT-ROOT ;

: LEAVE-FRAME ( n -- )
   -1 ACTIVE +! ACTIVE @ <> if 1 WRONG +! then AT-ROOT ;
;package
