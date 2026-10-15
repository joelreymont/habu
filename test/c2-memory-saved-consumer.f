\ Fresh source consumer of the saved unique C2 effect; wrapper source is absent.
require src/lib/test.f
require src/lib/memory.f
require src/lib/test/subject.f

package C2-MEMORY-SAVED-CONSUMER
private

: STEP ( n mut-view<p,p,a,u8> -- n mut-view<p,p,a,u8> )
   swap 1+ swap ;

: TWICE-RESULT ( -- n )
   40 [: STEP ;] C2-MEMORY-WRAPPER:TWICE ;

: CHANGE-BODY ( n mut-view<p,p,a,u8> -- bool mut-view<p,p,a,u8> )
   swap drop
   0 255 C2-MEM:MUT-BYTE!
   0 C2-MEM:MUT-BYTE@ swap 255 = swap ;

: CHANGE-RESULT ( -- bool )
   7 [: CHANGE-BODY ;] C2-MEMORY-WRAPPER:CHANGE ;

: LOAN-BODY ( mut-view<p,l,a,u8> -- n mut-view<p,l,a,u8> )
   0 31 C2-MEM:MUT-BYTE! 0 C2-MEM:MUT-BYTE@ ;

: LOAN-RESULT ( mut-view<p,q,a,u8> -- n mut-view<p,q,a,u8> )
   [: LOAN-BODY ;] C2-MEMORY-WRAPPER:ONCE-MUT-LOAN ;

: FRESH-LOAN ( -- n )
   1 MEM:BYTES-ALLOC-LEN [: LOAN-RESULT ;] C2-MEM:WITH-MUT ;

: READ-BODY ( read-view<p,l,u8> -- n read-view<p,l,u8> )
   0 C2-MEM:BYTE@ ;

: READ-RESULT ( mut-view<p,q,a,u8> -- n mut-view<p,q,a,u8> )
   [: READ-BODY ;] C2-MEMORY-WRAPPER:ONCE-READ ;

: FRESH-READ ( -- n )
   1 MEM:BYTES-ALLOC-LEN [: READ-RESULT ;] C2-MEM:WITH-MUT ;

: READ-AGAIN ( mut-view<p,q,a,u8> -- n mut-view<p,q,a,u8> )
   0 65 C2-MEM:MUT-BYTE!
   [: READ-BODY ;] C2-MEMORY-WRAPPER:ONCE-READ swap >r
   [: READ-BODY ;] C2-MEMORY-WRAPPER:ONCE-READ swap r> + swap ;

: FRESH-READ-AGAIN ( -- n )
   1 MEM:BYTES-ALLOC-LEN [: READ-AGAIN ;] C2-MEM:WITH-MUT ;

public

: RUN ( -- )
   T-RESET
   s" the saved wrapper creates two fresh owners" T-LABEL
   TWICE-RESULT 42 T=
   s" the saved wrapper preserves a changing ambient row" T-LABEL
   CHANGE-RESULT TTRUE
   s" the saved wrapper introduces a fresh exclusive loan" T-LABEL
   FRESH-LOAN 31 T=
   s" the saved wrapper introduces a fresh shared loan" T-LABEL
   FRESH-READ 0 T=
   s" the saved wrapper opens a fresh child on each call" T-LABEL
   FRESH-READ-AGAIN 130 T=
   s" a fixed quotation cannot impersonate the saved owner scheme" T-LABEL
   s" : C2-SAVED-MUT-MONO ( n [ n mut-view<q,q,a,u8> -- n mut-view<q,q,a,u8> | U -- U ] | U -- n | U ) C2-MEMORY-WRAPPER:TWICE ;"
   70 s" C2-MEMORY-WRAPPER:TWICE" SUBJECT:REFUSED? TTRUE
   s" a fixed shared callback cannot impersonate a saved child scheme" T-LABEL
   s" : C2-SAVED-READ-MONO ( mut-view<p,q,a,u8> [ read-view<p,q,u8> -- read-view<p,q,u8> | U -- U ] | U -- mut-view<p,q,a,u8> | U ) C2-MEMORY-WRAPPER:ONCE-READ ;"
   70 SUBJECT:EXITED? TTRUE
   s" a saved loan wrapper carries its callback exception edge" T-LABEL
   s" : C2-SAVED-THROW ( mut-view<p,l,a,u8> -- mut-view<p,l,a,u8> ) 1 throw ; : C2-SAVED-LOAN ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) [: C2-SAVED-THROW ;] C2-MEMORY-WRAPPER:ONCE-MUT-LOAN ; : C2-SAVED-CATCH ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) [: C2-SAVED-LOAN ;] catch drop 0 C2-MEM:MUT-BYTE@ swap drop ;"
   70 s" stale cell" SUBJECT:REFUSED? TTRUE
   s" saved owner and loan effects remain queryable" T-LABEL
   s" C2-MEM:WITH-MUT" EFFECT-QUERY TTRUE
   s" C2-MEM:WITH-READ" EFFECT-QUERY TTRUE
   T-REPORT
   s" c2-memory-saved-consumer: ok" type cr ;

;package

C2-MEMORY-SAVED-CONSUMER:RUN
