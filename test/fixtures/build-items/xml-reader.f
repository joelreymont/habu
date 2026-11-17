\ xml-reader.f - a program that requires the XML reader, which requires
\ src/lib/c2-memory.f, whose package a build seals before program source.

require src/lib/memory.f
require src/lib/c2-bytes.f
require src/lib/xml/c2.f

package BITEM-XML-READER
private
: DOC$ ( -- ^u8 n ) s" <a/>" ;

: FIRST ( mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> -- read-view<p,q,u8> mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> )
   XML-C2:NEXT drop XML-C2:NAME$ swap ;

: FIRST-STORAGE ( read-view<p,q,u8> mut-view<b,l,a,u8> -- read-view<p,q,u8> mut-view<b,l,a,u8> )
   swap [: FIRST ;] XML-C2:WITH-READER ;

: FIRST-NAME ( read-view<p,q,u8> -- read-view<p,q,u8> )
   XML:CURSOR-BYTES cell+ MEM:BYTES-ALLOC-LEN
   [: FIRST-STORAGE ;] C2-MEM:WITH-MUT ;

: TYPE-NAME ( read-view<p,q,u8> -- )
   C2-BYTES:LENGTH swap  0 ?do  i C2-MEM:BYTE@ swap emit  loop  drop ;

: PRINT-SOURCE ( read-view<p,q,u8> -- read-view<p,q,u8> )
   dup FIRST-NAME TYPE-NAME ;

: PRINT-ROOT ( mut-view<x,y,z,u8> -- mut-view<x,y,z,u8> )
   DOC$ C2-BYTES:COPY$ drop
   [: PRINT-SOURCE ;] C2-MEM:WITH-READ ;

public
: PRINT ( -- )
   DOC$ nip MEM:BYTES-ALLOC-LEN [: PRINT-ROOT ;] C2-MEM:WITH-MUT ;
;package

: MAIN ( -- ) s" bitem-xml-reader: " type BITEM-XML-READER:PRINT cr ;
