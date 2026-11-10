\ kept-docs.f - the file kept-main.f requires: its top level reads word docs from
\ a text it allocated, keeps their count in KEPT and frees the text; CHECK, run in
\ kept-main.f's program, reads a second text and prints both counts.

require src/lib/memory.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/word-docs.f

package FILE-OBJECTS-KEPT-DOCS
private

: FIRST$ ( -- ptr u8 n )
   s" package P public : A ( -- ) ; : B ( n -- n ) ; ;package" ;

\ The text lies in memory the build does not own, as a program's input does.
: READ-COPY ( -- n )
   FIRST$ {: s:ptr u:n :}
   u MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: a:ptr :}
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
