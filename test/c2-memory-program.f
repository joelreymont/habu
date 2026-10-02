\ Real unique C2 storage exercised through the dedicated native image.
require lib/test.f
require lib/memory.f
require lib/c2-memory.f
require lib/c2-bytes.f

package C2-MEMORY-PROGRAM
private

: EDGES ( mut-view<p,p,a,u8> -- bool mut-view<p,p,a,u8> )
   0 0 C2-MEM:MUT-BYTE!
   1 255 C2-MEM:MUT-BYTE!
   0 C2-MEM:MUT-BYTE@ swap 0 = >r
   1 C2-MEM:MUT-BYTE@ swap 255 = r> and swap ;

: EDGE-RESULT ( -- bool )
   2 MEM:BYTES-ALLOC-LEN [: EDGES ;] C2-MEM:WITH-MUT ;

: INNER-BYTE ( mut-view<p,p,a,u8> -- n mut-view<p,p,a,u8> )
   0 42 C2-MEM:MUT-BYTE! 0 C2-MEM:MUT-BYTE@ ;

: NESTED-OWNER ( mut-view<p,q,a,u8> -- n mut-view<p,q,a,u8> )
   1 MEM:BYTES-ALLOC-LEN [: INNER-BYTE ;] C2-MEM:WITH-MUT swap ;

: NESTED-RESULT ( -- n )
   2 MEM:BYTES-ALLOC-LEN [: NESTED-OWNER ;] C2-MEM:WITH-MUT ;

: MUT-CHILD ( mut-view<p,l,a,u8> -- n mut-view<p,l,a,u8> )
   0 73 C2-MEM:MUT-BYTE! 0 C2-MEM:MUT-BYTE@ ;

: RESTORED-MUT ( mut-view<p,q,a,u8> -- n mut-view<p,q,a,u8> )
   [: MUT-CHILD ;] C2-MEM:WITH-MUT-LOAN
   swap drop 0 C2-MEM:MUT-BYTE@ ;

: MUT-LOAN-RESULT ( -- n )
   1 MEM:BYTES-ALLOC-LEN [: RESTORED-MUT ;] C2-MEM:WITH-MUT ;

: MUT-LOAN-CATCH ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> )
   [: MUT-CHILD ;] C2-MEM:WITH-MUT-LOAN swap drop ;

: MUT-CATCH-OWNER ( mut-view<p,q,a,u8> -- n mut-view<p,q,a,u8> )
   [: MUT-LOAN-CATCH ;] catch dup 0= if drop else throw then
   0 C2-MEM:MUT-BYTE@ ;

: MUT-CATCH-RESULT ( -- n )
   1 MEM:BYTES-ALLOC-LEN [: MUT-CATCH-OWNER ;] C2-MEM:WITH-MUT ;

: READ-CHILD ( read-view<p,l,u8> -- n read-view<p,l,u8> )
   0 C2-MEM:BYTE@ ;

: NESTED-READ ( read-view<p,q,u8> -- n read-view<p,q,u8> )
   [: READ-CHILD ;] C2-MEM:WITH-READ ;

: RESTORED-SHARED ( mut-view<p,q,a,u8> -- bool mut-view<p,q,a,u8> )
   0 89 C2-MEM:MUT-BYTE!
   [: NESTED-READ ;] C2-MEM:WITH-READ
   swap 89 = >r
   0 C2-MEM:MUT-BYTE@ swap 89 = r> and swap ;

: SHARED-LOAN-RESULT ( -- bool )
   1 MEM:BYTES-ALLOC-LEN [: RESTORED-SHARED ;] C2-MEM:WITH-MUT ;

: READ-BOUND-CHILD ( read-view<p,l,u8> -- bool read-view<p,l,u8> )
   1 1 C2-BYTES:SLICE C2-BYTES:LENGTH {: size:n piece :}
   piece 0 C2-MEM:BYTE@ {: edge:u8 returned :}
   size 1 = edge 66 = and returned ;

: RESTORED-BOUND ( mut-view<p,q,a,u8> -- bool mut-view<p,q,a,u8> )
   0 17 C2-MEM:MUT-BYTE!
   1 66 C2-MEM:MUT-BYTE!
   [: READ-BOUND-CHILD ;] C2-MEM:WITH-READ
   swap >r
   C2-BYTES:MUT-LENGTH swap 2 = >r
   0 C2-MEM:MUT-BYTE@ swap 17 = >r
   1 C2-MEM:MUT-BYTE@ swap 66 =
   r> and r> and r> and swap ;

: BOUND-RESULT ( -- bool )
   2 MEM:BYTES-ALLOC-LEN [: RESTORED-BOUND ;] C2-MEM:WITH-MUT ;

: READ-LOAN-CATCH ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> )
   [: READ-CHILD ;] C2-MEM:WITH-READ swap drop ;

: READ-CATCH-OWNER ( mut-view<p,q,a,u8> -- n mut-view<p,q,a,u8> )
   0 89 C2-MEM:MUT-BYTE!
   [: READ-LOAN-CATCH ;] catch dup 0= if drop else throw then
   0 C2-MEM:MUT-BYTE@ ;

: READ-CATCH-RESULT ( -- n )
   1 MEM:BYTES-ALLOC-LEN [: READ-CATCH-OWNER ;] C2-MEM:WITH-MUT ;

: REPEATED-LOAN ( mut-view<p,q,a,u8> -- n mut-view<p,q,a,u8> )
   [: MUT-CHILD ;] C2-MEM:WITH-MUT-LOAN swap drop
   [: MUT-CHILD ;] C2-MEM:WITH-MUT-LOAN swap drop
   0 C2-MEM:MUT-BYTE@ ;

: REPEATED-RESULT ( -- n )
   1 MEM:BYTES-ALLOC-LEN [: REPEATED-LOAN ;] C2-MEM:WITH-MUT ;

: REPEATED-READ ( mut-view<p,q,a,u8> -- n mut-view<p,q,a,u8> )
   0 65 C2-MEM:MUT-BYTE!
   [: READ-CHILD ;] C2-MEM:WITH-READ swap >r
   [: READ-CHILD ;] C2-MEM:WITH-READ swap r> + swap ;

: REPEATED-READ-RESULT ( -- n )
   1 MEM:BYTES-ALLOC-LEN [: REPEATED-READ ;] C2-MEM:WITH-MUT ;

: READ-PARENT-COPY
   ( read-view<p,q,u8> read-view<p,l,u8> -- n read-view<p,q,u8> read-view<p,l,u8> )
   {: parent child :}
   parent 0 C2-MEM:BYTE@ {: left:u8 kept :}
   child 0 C2-MEM:BYTE@ {: right:u8 returned :}
   left right + kept returned ;

: PARENT-COPIES ( read-view<p,q,u8> -- n read-view<p,q,u8> )
   dup [: READ-PARENT-COPY ;] C2-MEM:WITH-READ
   {: sum:n copy restored :} copy drop sum restored ;

: PARENT-COPIES-BODY ( mut-view<p,p,a,u8> -- n mut-view<p,p,a,u8> )
   0 65 C2-MEM:MUT-BYTE!
   [: PARENT-COPIES ;] C2-MEM:WITH-READ ;

: PARENT-COPIES-RESULT ( -- n )
   1 MEM:BYTES-ALLOC-LEN [: PARENT-COPIES-BODY ;] C2-MEM:WITH-MUT ;

: RECURSE-RESULT ( n -- n )
   dup 0= if exit then
   1- 1 MEM:BYTES-ALLOC-LEN [: 0 C2-MEM:MUT-BYTE@ ;] C2-MEM:WITH-MUT drop
   RECURSE ;

: RETURN-RESULT ( n -- n )
   >r 1 MEM:BYTES-ALLOC-LEN [: 0 C2-MEM:MUT-BYTE@ ;] C2-MEM:WITH-MUT
   r> + ;

: OOB-OWNER-BODY ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   1 1 C2-MEM:MUT-BYTE! ;

: OOB-OWNER ( -- )
   1 MEM:BYTES-ALLOC-LEN [: OOB-OWNER-BODY ;] C2-MEM:WITH-MUT ;

: OOB-MUT-BODY ( mut-view<p,l,a,u8> -- mut-view<p,l,a,u8> )
   1 C2-MEM:MUT-BYTE@ swap drop ;

: OOB-MUT-LOAN-BODY ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> )
   [: OOB-MUT-BODY ;] C2-MEM:WITH-MUT-LOAN ;

: OOB-MUT-LOAN ( -- )
   1 MEM:BYTES-ALLOC-LEN [: OOB-MUT-LOAN-BODY ;] C2-MEM:WITH-MUT ;

: OOB-READ-BODY ( read-view<p,l,u8> -- read-view<p,l,u8> )
   -1 C2-MEM:BYTE@ swap drop ;

: OOB-READ-LOAN-BODY ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> )
   [: OOB-READ-BODY ;] C2-MEM:WITH-READ ;

: OOB-READ-LOAN ( -- )
   1 MEM:BYTES-ALLOC-LEN [: OOB-READ-LOAN-BODY ;] C2-MEM:WITH-MUT ;

: EMPTY-READ ( -- )
   0 MEM:BYTES-ALLOC-LEN [: 0 C2-MEM:MUT-BYTE@ ;] C2-MEM:WITH-MUT drop ;

: THROW-MUT-CHILD ( mut-view<p,l,a,u8> -- mut-view<p,l,a,u8> )
   -9367 throw ;

: THROW-MUT-ROOT ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   [: THROW-MUT-CHILD ;] C2-MEM:WITH-MUT-LOAN ;

: THROW-MUT ( -- )
   1 MEM:BYTES-ALLOC-LEN [: THROW-MUT-ROOT ;] C2-MEM:WITH-MUT ;

: THROW-READ-CHILD ( read-view<p,l,u8> -- read-view<p,l,u8> )
   -9368 throw ;

: THROW-READ-ROOT ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   [: THROW-READ-CHILD ;] C2-MEM:WITH-READ ;

: THROW-READ ( -- )
   1 MEM:BYTES-ALLOC-LEN [: THROW-READ-ROOT ;] C2-MEM:WITH-MUT ;

public

: RUN ( -- )
   T-RESET
   s" byte values 0 and 255 roundtrip through one unique owner" T-LABEL
   EDGE-RESULT TTRUE
   s" a nested allocation has a fresh owner and region" T-LABEL
   NESTED-RESULT 42 T=
   s" an exclusive loan writes and restores its original parent" T-LABEL
   MUT-LOAN-RESULT 73 T=
   s" a successful caught exclusive loan restores its parent" T-LABEL
   MUT-CATCH-RESULT 73 T=
   s" a nested shared loan reads and restores mutable authority" T-LABEL
   SHARED-LOAN-RESULT TTRUE
   s" returning a child restores the original parent bound" T-LABEL
   BOUND-RESULT TTRUE
   s" a successful caught shared loan restores its parent" T-LABEL
   READ-CATCH-RESULT 89 T=
   s" repeated child scopes restore the parent each time" T-LABEL
   REPEATED-RESULT 73 T=
   s" repeated shared loans each get a fresh child scope" T-LABEL
   REPEATED-READ-RESULT 130 T=
   s" a copied shared parent stays readable during its child loan" T-LABEL
   PARENT-COPIES-RESULT 130 T=
   s" recursive calls close their fresh owners" T-LABEL
   3 RECURSE-RESULT 0 T=
   s" an owner callback preserves an ambient return stack cell" T-LABEL
   9 RETURN-RESULT 9 T=
   s" a root write refuses its upper bound" T-LABEL
   [: OOB-OWNER ;] E-SPAN-RANGE TTHROWSQ
   s" an exclusive child read refuses its upper bound" T-LABEL
   [: OOB-MUT-LOAN ;] E-SPAN-RANGE TTHROWSQ
   s" a shared child read refuses a negative index" T-LABEL
   [: OOB-READ-LOAN ;] E-SPAN-RANGE TTHROWSQ
   s" a zero-size owner is rejected before it can expose a view" T-LABEL
   [: EMPTY-READ ;] E-MEM-SIZE TTHROWSQ
   s" an exclusive callback failure propagates through loan cleanup" T-LABEL
   [: THROW-MUT ;] -9367 TTHROWSQ
   s" a shared callback failure propagates through loan cleanup" T-LABEL
   [: THROW-READ ;] -9368 TTHROWSQ
   T-REPORT
   s" c2-memory-program: ok" type cr ;

;package

C2-MEMORY-PROGRAM:RUN
