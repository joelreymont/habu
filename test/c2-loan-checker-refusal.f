\ Syntax and authority refusals through the real evaluator load path.
require lib/test.f
require lib/test/subject.f

package C2-LOAN-CHECKER-REFUSAL
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
   s" a child binder needs a scope ceiling" T-LABEL
   s" : C2-BAD-CEILING ( forall<l inside n,[ read-view<p,l,u8> -- read-view<p,l,u8> ]> -- ) drop ;" 70 STATUS? TTRUE
   s" a child binder cannot use a region as its ceiling" T-LABEL
   s" : C2-REGION-CEILING ( forall-region<a,forall<l inside a,[ n -- n ]>> -- ) drop ;" 70 STATUS? TTRUE
   s" a child binder cannot publish its own scope as an ordinary result" T-LABEL
   s" : C2-BAD-OUTPUT ( forall<l inside q,[ read-view<p,l,u8> -- read-view<p,l,u8> ]> -- read-view<p,l,u8> ) drop 0 0 ;" 70 STATUS? TTRUE
   s" a mutable view requires a region identity" T-LABEL
   s" : C2-BAD-REGION ( mut-view<p,q,u8,u8> -- ) drop ;" 70 STATUS? TTRUE
   T-REPORT
   s" c2-loan-checker-refusal: ok" type cr ;
;package

C2-LOAN-CHECKER-REFUSAL:RUN
