\ Authenticated selection and the child field view cannot be forged or escaped.
require lib/test.f
require lib/test/subject.f
require lib/c2-memory.f
require test/c2-field-loan-wrapper.f

package C2-FIELD-LOAN-REFUSALS
public
STRUCTURE other 0 DERIVE init FIELD left n FIELD right n ;STRUCTURE
private

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

: STATUS? ( ptr u8 n n -- bool ) {: expected:n :}
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF expected = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   >r 2drop r> ;

public
: RUN ( -- )
   T-RESET
   s" a raw byte span cannot select an initialized field" T-LABEL
   s" : FL-RAW ( ptr u8 n -- ptr u8 n ) [: ;] C2-MEM:WITH-FIELD inner ;" 70 STATUS? TTRUE
   s" a shared view cannot select an initialized field" T-LABEL
   s" : FL-SHARED ( read-view<b,i,u8> -- read-view<b,i,u8> ) [: ;] C2-MEM:WITH-FIELD inner ;" 70 STATUS? TTRUE
   s" only a member of the committed receiver schema is accepted" T-LABEL
   s" : FL-UNKNOWN ( mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> -- mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> ) [: ;] C2-MEM:WITH-FIELD absent ;" 70 STATUS? TTRUE
   s" an outer scalar cannot be substituted for its Pair field" T-LABEL
   s" : FL-SCALAR-CB ( mut-view<b,j,c,init<i,C2-FIELD-LOAN-WRAPPER:pair>> -- mut-view<b,j,c,init<i,C2-FIELD-LOAN-WRAPPER:pair>> ) ; : FL-SCALAR ( mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> -- mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> ) [: FL-SCALAR-CB ;] C2-MEM:WITH-FIELD guard ;" 70 STATUS? TTRUE
   s" a same-layout different record cannot be a field callback" T-LABEL
   s" : FL-OTHER-CB ( mut-view<b,j,c,init<i,C2-FIELD-LOAN-REFUSALS:other>> -- mut-view<b,j,c,init<i,C2-FIELD-LOAN-REFUSALS:other>> ) ; : FL-OTHER ( mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> -- mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> ) [: FL-OTHER-CB ;] C2-MEM:WITH-FIELD inner ;" 70 STATUS? TTRUE
   s" a parent cannot be duplicated across its field loan" T-LABEL
   s" : FL-PARENT-COPY ( mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> -- mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> ) dup [: ;] C2-MEM:WITH-FIELD inner swap drop ;" 70 STATUS? TTRUE
   s" a child view cannot escape its callback" T-LABEL
   s" : FL-ESCAPE ( mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> -- mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> ) [: dup ;] C2-MEM:WITH-FIELD inner ;" 70 STATUS? TTRUE
   s" a caught throw cannot reopen a stale initialized parent" T-LABEL
   s" : FL-STALE-THROW ( mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> n -- mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> n ) swap 1 throw ; : FL-STALE ( mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> -- mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> ) 0 ['] FL-STALE-THROW catch drop drop [: ;] C2-MEM:WITH-FIELD inner ;" 70 STATUS? TTRUE
   s" a caught throwing field callback cannot reuse its parent" T-LABEL
   s" : FL-CB-THROW ( mut-view<b,j,c,init<i,C2-FIELD-LOAN-WRAPPER:pair>> -- mut-view<b,j,c,init<i,C2-FIELD-LOAN-WRAPPER:pair>> ) 1 throw ; : FL-TAKE ( mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> -- mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> ) [: FL-CB-THROW ;] C2-MEM:WITH-FIELD inner ; : FL-CATCH ( mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> -- mut-view<b,i,a,init<i,C2-FIELD-LOAN-WRAPPER:outer>> ) [: FL-TAKE ;] catch drop C2--FIELD--LOAN--WRAPPER-OUTER:GUARD@ drop ;" 70 STATUS? TTRUE
   s" the field keeps the parent's initialization lifetime" T-LABEL
   s" : FL-INIT ( mut-view<b,j,c,init<i,C2-FIELD-LOAN-WRAPPER:pair>> -- mut-view<b,j,c,init<j,C2-FIELD-LOAN-WRAPPER:pair>> ) ;" 70 STATUS? TTRUE
   s" scope entry tick cannot turn the field operation into a value" T-LABEL
   s" : FL-TICK ( -- ) ' C2-MEM:WITH-FIELD drop ;" 70 STATUS? TTRUE
   s" a scoped child view cannot be stored globally" T-LABEL
   s" TYPED-VARIABLE FL-GLOBAL mut-view<b,j,c,init<i,C2-FIELD-LOAN-WRAPPER:pair>>" 70 STATUS? TTRUE
   T-REPORT
   s" c2-field-loan-refusals: ok" type cr ;
;package

C2-FIELD-LOAN-REFUSALS:RUN
