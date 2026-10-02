\ Fresh consumer compiled against the saved XML-C2 image.
require lib/test.f
require lib/memory.f
require lib/xml/c2.f
require lib/c2-bytes.f
require test/c2-xml-consumer-program.f

package C2-XML-CONSUMER-SAVED
private
CAST: KIND>N ( XML:kind -- n )
CAST: XML-OFF>N ( off -- n )
CAST: XML-LEN>N ( len -- n )

: DOC$ ( -- ptr u8 n ) s" <r x='a&amp;b'>hi&amp;x</r>" ;

: CHECK-RAW ( mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> -- mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> )
   XML-C2:RAW XML-LEN>N 15 T= XML-OFF>N 0 T= ;

: CHECK-ATTR-RAW ( mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> -- mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> )
   XML-C2:ATTR-RAW XML-LEN>N 11 T= XML-OFF>N 3 T= ;

: CHECK-CONTENT ( mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> -- mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> )
   XML-C2:CONTENT XML-LEN>N 8 T= XML-OFF>N 15 T= ;

: CHECK-ATTR ( mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> mut-view<x,y,z,u8> -- bool mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> mut-view<x,y,z,u8> )
   XML-C2:ATTR-TEXT 3 = >r
   0 C2-MEM:MUT-BYTE@ swap $61 = >r
   1 C2-MEM:MUT-BYTE@ swap $26 = >r
   2 C2-MEM:MUT-BYTE@ swap $62 =
   r> and r> and r> and -rot ;

: CHECK-TEXT ( mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> mut-view<x,y,z,u8> -- bool mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> mut-view<x,y,z,u8> )
   XML-C2:TEXT 4 = >r
   0 C2-MEM:MUT-BYTE@ swap $68 = >r
   1 C2-MEM:MUT-BYTE@ swap $69 = >r
   2 C2-MEM:MUT-BYTE@ swap $26 = >r
   3 C2-MEM:MUT-BYTE@ swap $78 =
   r> and r> and r> and r> and -rot ;

: DOC-READER ( mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> -- bool mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> )
   XML-C2:NEXT KIND>N XML-KIND:START KIND>N T=
   CHECK-RAW
   XML-C2:ATTR-NEXT TTRUE
   CHECK-ATTR-RAW
   16 MEM:BYTES-ALLOC-LEN [: CHECK-ATTR ;] C2-MEM:WITH-MUT
   swap >r
   XML-C2:NEXT KIND>N XML-KIND:TEXT KIND>N T=
   CHECK-CONTENT
   16 MEM:BYTES-ALLOC-LEN [: CHECK-TEXT ;] C2-MEM:WITH-MUT
   swap r> and swap ;

: DOC-STORAGE ( read-view<p,q,u8> mut-view<b,l,a,u8> -- bool mut-view<b,l,a,u8> )
   swap [: DOC-READER ;] XML-C2:WITH-READER ;

: DOC-SOURCE ( read-view<p,q,u8> -- bool read-view<p,q,u8> )
   dup 2 XML:STORAGE-BYTES MEM:BYTES-ALLOC-LEN
   [: DOC-STORAGE ;] C2-MEM:WITH-MUT swap ;

: DOC-ROOT ( mut-view<x,y,z,u8> -- bool mut-view<x,y,z,u8> )
   DOC$ C2-BYTES:COPY$ drop
   [: DOC-SOURCE ;] C2-MEM:WITH-READ ;

: RESULT ( -- bool )
   DOC$ nip MEM:BYTES-ALLOC-LEN [: DOC-ROOT ;] C2-MEM:WITH-MUT ;

public
: RUN ( -- )
   T-RESET
   s" source coordinates and decoding retain lexical byte spans" T-LABEL
   RESULT TTRUE
   T-REPORT
   s" c2-xml-consumer-saved: ok" type cr ;
;package

C2-XML-CONSUMER-SAVED:RUN
