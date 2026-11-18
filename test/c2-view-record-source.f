split-str
\ A borrowed view moves by value through nested typed records.
require src/core/exit-status.f
require src/lib/test.f
require src/lib/adt/option.f
require src/lib/memory.f
require src/lib/c2-memory.f
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
      none OF EXIT-STATUS:FAILURE throw ENDOF
      some OF C2VINNER:UNMAKE 19 T= ENDOF
   ;MATCH ;

\ The view input supplies the scopes the result names, as a word's effect must.
: VACANT ( read-view<p,q,u8> -- c2vmaybe<p,q> read-view<p,q,u8> )
   C2VMAYBE:VACANT swap ;

: VACANT-ROUND ( read-view<p,q,u8> -- read-view<p,q,u8> )
   VACANT swap
   MATCH c2vmaybe
      held OF drop EXIT-STATUS:FAILURE throw ENDOF
      vacant OF ENDOF
   ;MATCH ;

: FIRST ( read-view<p,l,u8> -- n read-view<p,l,u8> )
   ROUND SUM-ROUND VACANT-ROUND 0 C2-MEM:BYTE@ ;

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
