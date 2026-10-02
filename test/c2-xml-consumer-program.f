\ Real C2 XML source and cursor scopes on the rooted native image.
require lib/test.f
require lib/memory.f
require lib/xml/c2.f
require lib/c2-bytes.f

package C2-XML-CONSUMER-PROGRAM
private

CAST: KIND>N ( XML:kind -- n )

: ASCII-DOC$ ( -- ptr u8 n ) s" <a/>" ;
: UTF8-DOC$ ( -- ptr u8 n ) s" <é/>" ;

: FIRST ( mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> -- read-view<p,q,u8> mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> )
   XML-C2:NEXT KIND>N XML-KIND:START KIND>N T=
   XML-C2:NAME$ swap ;

: FIRST-STORAGE ( read-view<p,q,u8> mut-view<b,l,a,u8> -- read-view<p,q,u8> mut-view<b,l,a,u8> )
   swap [: FIRST ;] XML-C2:WITH-READER ;

: FIRST-NAME ( read-view<p,q,u8> -- read-view<p,q,u8> )
   2 XML:STORAGE-BYTES MEM:BYTES-ALLOC-LEN
   [: FIRST-STORAGE ;] C2-MEM:WITH-MUT ;

: ASCII-NAME ( read-view<p,q,u8> -- bool read-view<p,q,u8> )
   C2-BYTES:LENGTH {: size:n name :}
   name 0 C2-MEM:BYTE@ {: first:u8 kept :}
   size 1 = first $61 = and kept ;

: SHARED-SOURCE ( read-view<p,q,u8> -- bool read-view<p,q,u8> )
   dup FIRST-NAME {: source first :}
   first ASCII-NAME drop {: first-ok:bool :}
   source FIRST-NAME ASCII-NAME drop {: second-ok:bool :}
   first-ok second-ok and source ;

: SHARED-ROOT ( mut-view<x,y,z,u8> -- bool mut-view<x,y,z,u8> )
   ASCII-DOC$ C2-BYTES:COPY$ drop
   [: SHARED-SOURCE ;] C2-MEM:WITH-READ ;

: SHARED-RESULT ( -- bool )
   ASCII-DOC$ nip MEM:BYTES-ALLOC-LEN [: SHARED-ROOT ;] C2-MEM:WITH-MUT ;

: SECOND-READER ( mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> mut-view<x,j,z,init<j,XML-C2:reader-state<p,q>>> -- bool mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> mut-view<x,j,z,init<j,XML-C2:reader-state<p,q>>> )
   swap XML-C2:KIND KIND>N XML-KIND:START KIND>N T= swap
   XML-C2:NEXT KIND>N XML-KIND:START KIND>N T=
   swap XML-C2:NEXT KIND>N XML-KIND:END KIND>N T= swap
   XML-C2:KIND KIND>N XML-KIND:START KIND>N T=
   true -rot ;

: SECOND-STORAGE ( read-view<p,q,u8> mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> mut-view<x,m,z,u8> -- bool mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> mut-view<x,m,z,u8> )
   rot [: SECOND-READER ;] XML-C2:WITH-READER ;

: OUTER-READER ( mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> -- bool mut-view<b,i,a,init<i,XML-C2:reader-state<p,q>>> )
   XML-C2:NEXT KIND>N XML-KIND:START KIND>N T=
   XML-C2:SOURCE$ swap
   2 XML:STORAGE-BYTES MEM:BYTES-ALLOC-LEN
   [: SECOND-STORAGE ;] C2-MEM:WITH-MUT ;

: OUTER-STORAGE ( read-view<p,q,u8> mut-view<b,l,a,u8> -- bool mut-view<b,l,a,u8> )
   swap [: OUTER-READER ;] XML-C2:WITH-READER ;

: CONCURRENT-SOURCE ( read-view<p,q,u8> -- bool read-view<p,q,u8> )
   dup 2 XML:STORAGE-BYTES MEM:BYTES-ALLOC-LEN
   [: OUTER-STORAGE ;] C2-MEM:WITH-MUT swap ;

: CONCURRENT-ROOT ( mut-view<x,y,z,u8> -- bool mut-view<x,y,z,u8> )
   ASCII-DOC$ C2-BYTES:COPY$ drop
   [: CONCURRENT-SOURCE ;] C2-MEM:WITH-READ ;

: CONCURRENT-RESULT ( -- bool )
   ASCII-DOC$ nip MEM:BYTES-ALLOC-LEN [: CONCURRENT-ROOT ;] C2-MEM:WITH-MUT ;

: UTF8-NAME ( read-view<p,q,u8> -- bool read-view<p,q,u8> )
   C2-BYTES:LENGTH {: size:n name :}
   name 0 C2-MEM:BYTE@ {: first:u8 kept :}
   kept 1 C2-MEM:BYTE@ {: second:u8 restored :}
   size 2 = first $C3 = and second $A9 = and restored ;

: UTF8-SOURCE ( read-view<p,q,u8> -- bool read-view<p,q,u8> )
   dup FIRST-NAME UTF8-NAME drop swap ;

: UTF8-ROOT ( mut-view<x,y,z,u8> -- bool mut-view<x,y,z,u8> )
   UTF8-DOC$ C2-BYTES:COPY$ drop
   [: UTF8-SOURCE ;] C2-MEM:WITH-READ ;

: UTF8-RESULT ( -- bool )
   UTF8-DOC$ nip MEM:BYTES-ALLOC-LEN [: UTF8-ROOT ;] C2-MEM:WITH-MUT ;

: CHECK-SLICE ( read-view<p,q,u8> -- bool read-view<p,q,u8> )
   dup 1 3 C2-BYTES:SLICE C2-BYTES:LENGTH {: size:n piece :}
   piece 0 C2-MEM:BYTE@ {: first:u8 kept :}
   kept 2 C2-MEM:BYTE@ {: last:u8 rest :}
   rest drop
   size 3 = first $62 = and last $64 = and >r
   dup 6 0 C2-BYTES:SLICE C2-BYTES:LENGTH {: zero-size:n empty :}
   empty drop zero-size 0= r> and >r
   dup 5 1 C2-BYTES:SLICE 0 C2-MEM:BYTE@ {: edge:u8 tail :}
   tail drop edge $66 = r> and >r
   dup dup C2-BYTES:EQUAL? r> and >r
   dup dup 0 1 C2-BYTES:SLICE swap 1 1 C2-BYTES:SLICE
   C2-BYTES:EQUAL? 0= r> and swap ;

: COPY-DEST ( read-view<p,q,u8> mut-view<x,y,z,u8> -- bool mut-view<x,y,z,u8> )
   swap C2-BYTES:COPY 6 = >r
   0 C2-MEM:MUT-BYTE@ swap $61 = >r
   3 C2-MEM:MUT-BYTE@ swap $64 = >r
   5 C2-MEM:MUT-BYTE@ swap $66 =
   r> and r> and r> and swap ;

: CHECK-BYTES ( read-view<p,q,u8> -- bool read-view<p,q,u8> )
   dup CHECK-SLICE drop >r
   dup 6 MEM:BYTES-ALLOC-LEN [: COPY-DEST ;] C2-MEM:WITH-MUT
   {: source copied:bool :}
   copied r> and source ;

: BYTE-ROOT ( mut-view<x,y,z,u8> -- bool mut-view<x,y,z,u8> )
   s" abcdef" C2-BYTES:COPY$ drop
   [: CHECK-BYTES ;] C2-MEM:WITH-READ ;

: BYTE-RESULT ( -- bool )
   6 MEM:BYTES-ALLOC-LEN [: BYTE-ROOT ;] C2-MEM:WITH-MUT ;

: PREFIX-BODY ( mut-view<p,q,a,u8> -- bool mut-view<p,q,a,u8> )
   3 C2-BYTES:PREFIX C2-BYTES:MUT-LENGTH swap 3 = >r
   2 90 C2-MEM:MUT-BYTE!
   2 C2-MEM:MUT-BYTE@ swap 90 = r> and swap ;

: PREFIX-RESULT ( -- bool )
   6 MEM:BYTES-ALLOC-LEN [: PREFIX-BODY ;] C2-MEM:WITH-MUT ;

: ZERO-PREFIX ( mut-view<p,q,a,u8> -- bool mut-view<p,q,a,u8> )
   0 C2-BYTES:PREFIX C2-BYTES:MUT-LENGTH swap 0= swap ;

: ZERO-RESULT ( -- bool )
   1 MEM:BYTES-ALLOC-LEN [: ZERO-PREFIX ;] C2-MEM:WITH-MUT ;

: BAD-PREFIX-BODY ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> )
   2 C2-BYTES:PREFIX ;

: BAD-PREFIX ( -- )
   1 MEM:BYTES-ALLOC-LEN [: BAD-PREFIX-BODY ;] C2-MEM:WITH-MUT ;

: PREFIX-OOB-BODY ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> )
   3 C2-BYTES:PREFIX 3 11 C2-MEM:MUT-BYTE! ;

: PREFIX-OOB ( -- )
   6 MEM:BYTES-ALLOC-LEN [: PREFIX-OOB-BODY ;] C2-MEM:WITH-MUT ;

: BAD-SLICE-OFF ( read-view<p,q,u8> -- read-view<p,q,u8> )
   -1 1 C2-BYTES:SLICE ;

: BAD-SLICE-LEN ( read-view<p,q,u8> -- read-view<p,q,u8> )
   0 -1 C2-BYTES:SLICE ;

: BAD-SLICE-END ( read-view<p,q,u8> -- read-view<p,q,u8> )
   1 1 C2-BYTES:SLICE ;

: BAD-SLICE-MAX ( read-view<p,q,u8> -- read-view<p,q,u8> )
   MEM-MAX-N 1 C2-BYTES:SLICE ;

: BAD-OFF-ROOT ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> )
   [: BAD-SLICE-OFF ;] C2-MEM:WITH-READ ;

: BAD-LEN-ROOT ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> )
   [: BAD-SLICE-LEN ;] C2-MEM:WITH-READ ;

: BAD-END-ROOT ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> )
   [: BAD-SLICE-END ;] C2-MEM:WITH-READ ;

: BAD-MAX-ROOT ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> )
   [: BAD-SLICE-MAX ;] C2-MEM:WITH-READ ;

: BAD-OFF ( -- )
   1 MEM:BYTES-ALLOC-LEN [: BAD-OFF-ROOT ;] C2-MEM:WITH-MUT ;

: BAD-LEN ( -- )
   1 MEM:BYTES-ALLOC-LEN [: BAD-LEN-ROOT ;] C2-MEM:WITH-MUT ;

: BAD-END ( -- )
   1 MEM:BYTES-ALLOC-LEN [: BAD-END-ROOT ;] C2-MEM:WITH-MUT ;

: BAD-MAX ( -- )
   1 MEM:BYTES-ALLOC-LEN [: BAD-MAX-ROOT ;] C2-MEM:WITH-MUT ;

: BAD-COPY-DEST ( read-view<p,q,u8> mut-view<x,y,z,u8> -- mut-view<x,y,z,u8> )
   swap C2-BYTES:COPY drop ;

: BAD-COPY-SOURCE ( read-view<p,q,u8> -- read-view<p,q,u8> )
   dup 1 MEM:BYTES-ALLOC-LEN [: BAD-COPY-DEST ;] C2-MEM:WITH-MUT ;

: BAD-COPY-ROOT ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> )
   [: BAD-COPY-SOURCE ;] C2-MEM:WITH-READ ;

: BAD-COPY ( -- )
   6 MEM:BYTES-ALLOC-LEN [: BAD-COPY-ROOT ;] C2-MEM:WITH-MUT ;

public
: RUN ( -- )
   T-RESET
   s" two cursors read one copied source; a projected name lives after each cursor" T-LABEL
   SHARED-RESULT TTRUE
   s" two live cursors advance independently over one source view" T-LABEL
   CONCURRENT-RESULT TTRUE
   s" UTF-8 scalar names retain their original source bytes" T-LABEL
   UTF8-RESULT TTRUE
   s" bounded slices and view copies read the copied source" T-LABEL
   BYTE-RESULT TTRUE
   s" mutable prefix retains unique authority over its allocation" T-LABEL
   PREFIX-RESULT TTRUE
   s" a one-byte allocation can expose a zero-length mutable view" T-LABEL
   ZERO-RESULT TTRUE
   s" prefix refuses an extent larger than the allocation" T-LABEL
   [: BAD-PREFIX ;] E-SPAN-RANGE TTHROWSQ
   s" narrowed mutable prefix refuses access beyond its new bound" T-LABEL
   [: PREFIX-OOB ;] E-SPAN-RANGE TTHROWSQ
   s" slice refuses negative, over-end, and maximum-cell offsets" T-LABEL
   [: BAD-OFF ;] E-SPAN-RANGE TTHROWSQ
   [: BAD-LEN ;] E-SPAN-RANGE TTHROWSQ
   [: BAD-END ;] E-SPAN-RANGE TTHROWSQ
   [: BAD-MAX ;] E-SPAN-RANGE TTHROWSQ
   s" view copy refuses a destination shorter than the source" T-LABEL
   [: BAD-COPY ;] E-SPAN-CAPACITY TTHROWSQ
   T-REPORT
   s" c2-xml-consumer-program: ok" type cr ;
;package

C2-XML-CONSUMER-PROGRAM:RUN
