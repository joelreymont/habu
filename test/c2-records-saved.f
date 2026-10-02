\ Fresh saved-image consumer of table and element loan effects.
require lib/test.f
require lib/memory.f
require lib/c2-memory.f

package C2-RECORDS-SAVED
private

: EDIT ( mut-view<b,j,a,init<i,C2-RECORDS-WRAPPER:pair>> -- mut-view<b,j,a,init<i,C2-RECORDS-WRAPPER:pair>> )
   33 C2--RECORDS--WRAPPER-PAIR:LEFT! ;

: STEP ( n records<b,i,a,C2-RECORDS-WRAPPER:pair> -- n records<b,i,a,C2-RECORDS-WRAPPER:pair> )
   0 [: EDIT ;] C2-RECORDS-WRAPPER:ELEMENT
   swap 1+ swap ;

: BODY ( mut-view<b,l,a,u8> -- n mut-view<b,l,a,u8> )
   41 swap
   1 11 22 C2--RECORDS--WRAPPER-PAIR:MAKE
   [: STEP ;] C2-RECORDS-WRAPPER:APPLY ;

: RESULT ( -- n )
   16 MEM:BYTES-ALLOC-LEN [: BODY ;] C2-MEM:WITH-MUT ;

public

: RUN ( -- )
   T-RESET
   s" a saved table wrapper introduces a fresh init and element scope" T-LABEL
   RESULT 42 T=
   T-REPORT
   s" c2-records-saved: ok" type cr ;

;package

C2-RECORDS-SAVED:RUN
