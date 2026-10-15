\ A new consumer is checked against a wrapper captured in another image.
require src/lib/test.f
require src/lib/memory.f
require src/lib/c2-memory.f
require src/lib/test/subject.f

package C2-INIT-SAVED-CONSUMER
private

: STEP ( n mut-view<p,i,a,init<i,C2-INIT-WRAPPER:c2isaved>> -- n mut-view<p,i,a,init<i,C2-INIT-WRAPPER:c2isaved>> )
   C2--INIT--WRAPPER-C2ISAVED:LEFT@ 11 <> if -7311 throw then
   C2--INIT--WRAPPER-C2ISAVED:RIGHT@ 22 <> if -7312 throw then
   33 C2--INIT--WRAPPER-C2ISAVED:LEFT!
   C2--INIT--WRAPPER-C2ISAVED:LEFT@ 33 <> if -7313 throw then
   swap 3 + swap ;

: BODY ( n mut-view<p,l,a,u8> -- n mut-view<p,l,a,u8> )
   11 22 C2--INIT--WRAPPER-C2ISAVED:MAKE [: STEP ;] C2-INIT-WRAPPER:APPLY ;

: RESULT ( -- n )
   39 16 MEM:BYTES-ALLOC-LEN [: BODY ;] C2-MEM:WITH-MUT ;

public

: RUN ( -- )
   T-RESET
   s" a fresh saved consumer opens both initialized bounds" T-LABEL
   RESULT 42 T=
   s" a saved setter cannot store an inner-scope view in an outer field" T-LABEL
   s" : C2-INIT-INNER-STORE ( mut-view<p,i,a,init<i,C2-INIT-WRAPPER:c2isource<p,l>>> read-view<p,i,u8> -- mut-view<p,i,a,init<i,C2-INIT-WRAPPER:c2isource<p,l>>> ) C2--INIT--WRAPPER-C2ISOURCE:SOURCE! ;"
   70 SUBJECT:EXITED? TTRUE
   T-REPORT
   s" c2-init-saved-consumer: ok" type cr ;

;package

C2-INIT-SAVED-CONSUMER:RUN
