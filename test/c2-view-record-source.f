\ A borrowed view moves by value through nested typed records.
require lib/test.f
require lib/adt/option.f
require lib/memory.f
require lib/c2-memory.f
require test/c2-view-record-defs.f

package C2-VIEW-RECORD
private

: ROUND ( read-view<p,q,u8> -- read-view<p,q,u8> )
   17 C2VINNER:MAKE C2VOUTER:MAKE
   C2VOUTER:UNMAKE C2VINNER:UNMAKE
   17 T= ;

: TYPED-INNER ( read-view<p,q,t> -- read-view<p,q,t> )
   C2VTYPEDINNER:MAKE C2VTYPEDINNER:UNMAKE ;

: TYPED-ROUND ( c2vtypedinner<p,q,c2vpair> -- c2vtypedinner<p,q,c2vpair> )
   C2VTYPEDOUTER:MAKE C2VTYPEDOUTER:UNMAKE ;

: TYPED-WIDE-UNMAKE ( c2vtypedinner<p,q,c2vpair> -- read-view<p,q,c2vpair> )
   C2VTYPEDINNER:UNMAKE ;

: TYPED-WIDE-ROUND ( read-view<p,q,c2vpair> -- read-view<p,q,c2vpair> )
   C2VTYPEDINNER:MAKE TYPED-WIDE-UNMAKE ;

: SUM-WRAP ( read-view<p,q,u8> -- option<c2vinner<p,q>> )
   19 C2VINNER:MAKE OPTION:SOME ;

: SUM-ROUND ( read-view<p,q,u8> -- read-view<p,q,u8> )
   SUM-WRAP
   MATCH option
      none OF 1 throw ENDOF
      some OF C2VINNER:UNMAKE 19 T= ENDOF
   ;MATCH ;

: FIRST ( read-view<p,l,u8> -- n read-view<p,l,u8> )
   ROUND SUM-ROUND 0 C2-MEM:BYTE@ ;

: BORROW ( mut-view<p,p,a,u8> -- n mut-view<p,p,a,u8> )
   0 65 C2-MEM:MUT-BYTE!
   [: FIRST ;] C2-MEM:WITH-READ ;

public

: RUN ( -- )
   T-RESET
   s" nested by-value records preserve a live source scope" T-LABEL
   1 MEM:BYTES-ALLOC-LEN [: BORROW ;] C2-MEM:WITH-MUT 65 T=
   T-REPORT
   s" c2-view-record-source: ok" type cr ;

;package

C2-VIEW-RECORD:RUN
