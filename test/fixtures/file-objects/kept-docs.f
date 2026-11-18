split-str
\ kept-docs.f - the file kept-main.f requires.

require src/lib/memory.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/word-docs.f

package FILE-OBJECTS-KEPT-DOCS
private

: FIRST$ ( -- str )
   s" package P public : A ( -- ) ; : B ( n -- n ) ; ;package" ;

\ The text lies in memory the build does not own, as a program's input does.
: READ-COPY ( -- n )
   FIRST$ {: s:^ u:n :}
   u MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: a:^ :}
   s a u BYTE-COPY
   a u WORD-DOCS:FILE
   WORD-DOCS:COUNT
   a u MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   WORD-DOCS:RESET ;

READ-COPY constant KEPT

public
: CHECK ( -- )
   s" : C ( -- ) ;" WORD-DOCS:FILE
   ." docs " KEPT FMT:.INT space WORD-DOCS:COUNT FMT:.INT cr ;
;package
