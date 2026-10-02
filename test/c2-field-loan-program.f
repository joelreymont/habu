\ A nested initialized record lends its embedded Pair for an in-place edit.
require lib/test.f
require lib/memory.f
require lib/c2-memory.f

package C2-FIELD-LOAN-PROGRAM
public
STRUCTURE pair 0 DERIVE init FIELD left n FIELD right n ;STRUCTURE
STRUCTURE outer 0 DERIVE init
   FIELD marker n
   FIELD inner pair
   FIELD guard n
;STRUCTURE

private
: CHANGE-PAIR ( mut-view<p,j,a,init<i,pair>> -- mut-view<p,j,a,init<i,pair>> )
   41 C2--FIELD--LOAN--PROGRAM-PAIR:LEFT! ;

: EDIT-OUTER ( mut-view<p,i,a,init<i,outer>> -- bool mut-view<p,i,a,init<i,outer>> )
   [: ;] C2-MEM:WITH-FIELD marker
   [: CHANGE-PAIR ;] C2-MEM:WITH-FIELD inner
   C2--FIELD--LOAN--PROGRAM-OUTER:INNER@ {: pair :}
   pair C2--FIELD--LOAN--PROGRAM-PAIR:UNMAKE drop 41 =
   >r C2--FIELD--LOAN--PROGRAM-OUTER:GUARD@ 73 = r> and
   >r C2--FIELD--LOAN--PROGRAM-OUTER:MARKER@ 17 = r> and swap ;

: EDIT-BYTES ( mut-view<p,l,a,u8> -- bool mut-view<p,l,a,u8> )
   17 11 29 C2--FIELD--LOAN--PROGRAM-PAIR:MAKE
   73 C2--FIELD--LOAN--PROGRAM-OUTER:MAKE
   [: EDIT-OUTER ;] C2-MEM:WITH-INIT
   swap >r 0 C2-MEM:MUT-BYTE@ swap 0= r> and swap ;

public
: RUN ( -- )
   T-RESET
   s" nested Pair is edited through a field loan and cleared on close" T-LABEL
   32 MEM:BYTES-ALLOC-LEN [: EDIT-BYTES ;] C2-MEM:WITH-MUT TTRUE
   T-REPORT
   s" c2-field-loan-program: ok" type cr ;
;package

C2-FIELD-LOAN-PROGRAM:RUN
