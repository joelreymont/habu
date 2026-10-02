\ Fresh consumer of an authenticated field-loan wrapper from a saved image.
require lib/test.f
require lib/memory.f

package C2-FIELD-LOAN-SAVED
private

: EDIT ( mut-view<b,j,c,init<i,C2-FIELD-LOAN-WRAPPER:pair>> -- mut-view<b,j,c,init<i,C2-FIELD-LOAN-WRAPPER:pair>> )
   41 C2--FIELD--LOAN--WRAPPER-PAIR:LEFT! ;

: OUTER ( mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> -- bool mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> )
   [: EDIT ;] C2-FIELD-LOAN-WRAPPER:APPLY
   C2--FIELD--LOAN--WRAPPER-OUTER:INNER@ {: pair :}
   pair C2--FIELD--LOAN--WRAPPER-PAIR:UNMAKE drop 41 =
   >r C2--FIELD--LOAN--WRAPPER-OUTER:GUARD@ 73 = r> and swap ;

: BODY ( mut-view<b,l,a,u8> -- bool mut-view<b,l,a,u8> )
   17 11 29 C2--FIELD--LOAN--WRAPPER-PAIR:MAKE
   73 C2--FIELD--LOAN--WRAPPER-OUTER:MAKE
   [: OUTER ;] C2-MEM:WITH-INIT ;

public
: RUN ( -- )
   T-RESET
   s" saved wrapper lends only the embedded Pair" T-LABEL
   32 MEM:BYTES-ALLOC-LEN [: BODY ;] C2-MEM:WITH-MUT TTRUE
   T-REPORT
   s" c2-field-loan-saved: ok" type cr ;
;package

C2-FIELD-LOAN-SAVED:RUN
