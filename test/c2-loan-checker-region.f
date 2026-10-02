\ A scope and a region binder can prefix one checked quotation.
package C2-LOAN-CHECKER-REGION
public

: DROP-SCHEME ( forall<p,forall-region<a,[ n -- n ]>> -- ) drop ;
: KEEP-MUT ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> ) ;
: DROP-MUT-SCHEME ( forall-region<a,[ R mut-view<p,q,a,u8> -- S mut-view<p,q,a,u8> ]> -- ) drop ;

;package

s" c2-loan-checker-region: ok" type cr
