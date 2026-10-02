\ The bounded callback is a checked value whose ceiling is the parent's scope.
\ Load this file with the candidate native image; its definitions are the artifact.

package C2-LOAN-CHECKER-POSITIVE
public

: KEEP-PARENT ( read-view<p,q,u8> -- read-view<p,q,u8> ) ;

: KEEP-CHILD ( read-view<p,l,u8> -- read-view<p,l,u8> ) ;

: DROP-CALLBACK ( read-view<p,q,u8> forall<l inside q,[ R read-view<p,l,u8> -- S read-view<p,l,u8> | U -- U ]> -- read-view<p,q,u8> ) drop ;

;package

s" c2-loan-checker-positive: ok" type cr
