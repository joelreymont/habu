\ The supplied binders may only resolve to the authenticated input identities
\ with the parent and second bounds carried by those identities.
require src/lib/test.f
require src/lib/test/subject.f
require test/c2-forall-input.f

package C2-FORALL-INPUT-REFUSALS
public
: RUN ( -- )
   T-RESET
   s" an opened child binder must use the live parent ceiling" T-LABEL
   s" package C2-FORALL-INPUT public : BAD-PARENT ( R C2-MEM:owner<p,i,a> forall-region<b,forall<j inside [q,state<p>], [ R control<p,i,a,j,b> -- S control<p,i,a,j,b> | U -- U ]>> | U -- S C2-MEM:owner<p,i,a> | U ) {: cb :} 16 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC swap cb rot SEED [: BODY ;] C2-MEM:WITH-INIT C2-MEM:PUBLISH drop ; ;package" 70 SUBJECT:EXITED? TTRUE
   s" an opened child binder must keep its value dependency" T-LABEL
   s" package C2-FORALL-INPUT public : BAD-SECOND ( R C2-MEM:owner<p,i,a> forall-region<b,forall<j inside [p,state<q>], [ R control<p,i,a,j,b> -- S control<p,i,a,j,b> | U -- U ]>> | U -- S C2-MEM:owner<p,i,a> | U ) {: cb :} 16 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC swap cb rot SEED [: BODY ;] C2-MEM:WITH-INIT C2-MEM:PUBLISH drop ; ;package" 70 SUBJECT:EXITED? TTRUE
   s" an unused region binder has no input authority" T-LABEL
   s" package C2-FORALL-INPUT public : UNRESOLVED ( R C2-MEM:owner<p,i,a> forall-region<b,forall<j inside [p,state<p>], [ R control<p,i,a,j,c> -- S control<p,i,a,j,c> | U -- U ]>> | U -- S C2-MEM:owner<p,i,a> | U ) {: cb :} 16 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC swap cb rot SEED [: BODY ;] C2-MEM:WITH-INIT C2-MEM:PUBLISH drop ; ;package" 70 SUBJECT:EXITED? TTRUE
   s" a surplus binder cannot be grounded by callback output" T-LABEL
   s" package C2-FORALL-INPUT public : TAKE-OUTPUT ( read-view<p,p,u8> [ -- read-view<p,p,u8> ] -- read-view<p,p,u8> ) drop ; : OUT-ONLY ( read-view<p,p,u8> forall<q,[ -- read-view<q,q,u8> ]> -- read-view<p,p,u8> ) TAKE-OUTPUT ; ;package" 70 SUBJECT:EXITED? TTRUE
   s" a callback input alone does not supply a generic scope" T-LABEL
   s" package C2-FORALL-INPUT public : TAKE-UNBOUND ( [ read-view<p,p,u8> -- read-view<p,p,u8> ] -- ) drop ; : UNBOUND ( forall<q,[ read-view<q,q,u8> -- read-view<q,q,u8> ]> -- ) TAKE-UNBOUND ; ;package" 70 SUBJECT:EXITED? TTRUE
   s" a callback input alone does not supply a generic region" T-LABEL
   s" package C2-FORALL-INPUT public : TAKE-UNBOUND-REGION ( [ mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> ] -- ) drop ; : UNBOUND-REGION ( forall-region<b,[ mut-view<p,p,b,u8> -- mut-view<p,p,b,u8> ]> -- ) TAKE-UNBOUND-REGION ; ;package" 70 SUBJECT:EXITED? TTRUE
   s" an unbounded generic scope cannot satisfy a child ceiling" T-LABEL
   s" package C2-FORALL-INPUT public : BOUND-GENERIC ( read-view<p,p,u8> forall<q inside p,[ read-view<p,q,u8> -- read-view<p,q,u8> ]> -- read-view<p,p,u8> ) TAKE-SCOPE ; ;package" 70 SUBJECT:EXITED? TTRUE
   s" a view borrowed after surplus specialization cannot leave its loan" T-LABEL
   s" package C2-FORALL-INPUT public : LOAN-ESCAPE ( R read-view<p,p,u8> forall<q,[ read-view<q,q,u8> -- read-view<q,q,u8> ]> forall<j inside p,[ read-view<p,j,u8> -- read-view<p,j,u8> read-view<p,j,u8> ]> | U -- S read-view<p,p,u8> | U ) {: loan :} GENERIC-SCOPE loan C2-MEM:WITH-READ ; ;package"
   70 s" at 'C2-MEM:WITH-READ'" SUBJECT:REFUSED? TTRUE
   T-REPORT
   s" c2-forall-input-refusals: ok" type cr ;
;package

C2-FORALL-INPUT-REFUSALS:RUN
