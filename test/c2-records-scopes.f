\ Stored source and decoded views keep their independent owner dependencies.
\ A nested cursor scope closes before an element getter reads both fields.
require lib/test.f
require lib/memory.f
require lib/c2-memory.f
require test/c2-records-types.f

package C2-RECORDS-SCOPES
private

: SOURCE-VAL
   ( mut-view<b,j,a,init<i,C2-RECORDS-TYPES:shelf<p,q,t,u>>> -- n mut-view<b,j,a,init<i,C2-RECORDS-TYPES:shelf<p,q,t,u>>> )
   C2--RECORDS--TYPES-SHELF:SOURCE@ 0 C2-MEM:BYTE@ drop swap ;

: DECODED-VAL
   ( mut-view<b,j,a,init<i,C2-RECORDS-TYPES:shelf<p,q,t,u>>> -- n mut-view<b,j,a,init<i,C2-RECORDS-TYPES:shelf<p,q,t,u>>> )
   C2--RECORDS--TYPES-SHELF:DECODED@ 0 C2-MEM:BYTE@ drop swap ;

: FIELDS
   ( mut-view<b,j,a,init<i,C2-RECORDS-TYPES:shelf<p,q,t,u>>> -- n mut-view<b,j,a,init<i,C2-RECORDS-TYPES:shelf<p,q,t,u>>> )
   SOURCE-VAL swap >r
   DECODED-VAL swap >r
   r> r> + swap ;

: CURSOR
   ( records<b,i,a,C2-RECORDS-TYPES:shelf<p,q,t,u>> mut-view<c,c,d,u8> -- records<b,i,a,C2-RECORDS-TYPES:shelf<p,q,t,u>> mut-view<c,c,d,u8> )
   0 7 C2-MEM:MUT-BYTE! ;

: TABLE
   ( records<b,i,a,C2-RECORDS-TYPES:shelf<p,q,t,u>> -- n records<b,i,a,C2-RECORDS-TYPES:shelf<p,q,t,u>> )
   1 MEM:BYTES-ALLOC-LEN [: CURSOR ;] C2-MEM:WITH-MUT
   0 [: FIELDS ;] C2-MEM:WITH-RECORD ;

: TABLE-OWNER
   ( read-view<p,q,u8> read-view<t,u,u8> mut-view<b,b,a,u8> -- n mut-view<b,b,a,u8> )
   -rot {: source decoded :}
   1 source decoded 0 C2--RECORDS--TYPES-SHELF:MAKE
   [: TABLE ;] C2-MEM:WITH-RECORDS ;

: TREE-READ
   ( read-view<p,q,u8> read-view<t,u,u8> -- n read-view<p,q,u8> read-view<t,u,u8> )
   {: source decoded :}
   source decoded 40 MEM:BYTES-ALLOC-LEN [: TABLE-OWNER ;] C2-MEM:WITH-MUT
   source decoded ;

: DECODE-OWNER
   ( read-view<p,q,u8> mut-view<t,t,c,u8> -- n read-view<p,q,u8> mut-view<t,t,c,u8> )
   0 66 C2-MEM:MUT-BYTE!
   [: TREE-READ ;] C2-MEM:WITH-READ ;

: SOURCE-LOAN ( read-view<p,q,u8> -- n read-view<p,q,u8> )
   1 MEM:BYTES-ALLOC-LEN [: DECODE-OWNER ;] C2-MEM:WITH-MUT ;

: SOURCE-OWNER ( mut-view<p,p,a,u8> -- n mut-view<p,p,a,u8> )
   0 65 C2-MEM:MUT-BYTE!
   [: SOURCE-LOAN ;] C2-MEM:WITH-READ ;

: RESULT ( -- n )
   1 MEM:BYTES-ALLOC-LEN [: SOURCE-OWNER ;] C2-MEM:WITH-MUT ;

public

: RUN ( -- )
   T-RESET
   s" table fields retain source and decoded scopes after cursor close" T-LABEL
   RESULT 131 T=
   T-REPORT
   s" c2-records-scopes: ok" type cr ;

;package

C2-RECORDS-SCOPES:RUN
