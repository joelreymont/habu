\ Initialized storage keeps its scope and nominal element type through the real
\ checker load path.

require src/lib/test.f
require src/lib/test/subject.f

package C2-INIT-SCHEMA

public
PRODUCT pair FIELD left n FIELD right n ;PRODUCT
private

: KEEP ( init<p,pair> -- init<p,pair> ) ;
public
: KEEP-WIDE ( init<p,pair> -- init<p,pair> ) KEEP ;

: CHECK ( -- )
   s" an initialized wide record retains its nominal type" T-LABEL
   s" : C2-INIT-WIDE ( init<p,C2-INIT-SCHEMA:pair> -- init<p,C2-INIT-SCHEMA:pair> ) C2-INIT-SCHEMA:KEEP-WIDE ;" 0 SUBJECT:EXITED? TTRUE
   s" a different lifetime cannot be substituted" T-LABEL
   s" : C2-INIT-SCOPE-ERASE ( init<p,C2-INIT-SCHEMA:pair> -- init<q,C2-INIT-SCHEMA:pair> ) ;" 70 SUBJECT:EXITED? TTRUE
   s" the element's own scope dependency survives the wrapper" T-LABEL
   s" : C2-INIT-NESTED ( init<p,read-view<q,q,u8>> -- init<p,read-view<q,q,u8>> ) ;" 0 SUBJECT:EXITED? TTRUE
   s" the element's scope dependency cannot be substituted" T-LABEL
   s" : C2-INIT-NESTED-ERASE ( init<p,read-view<q,q,u8>> -- init<p,read-view<r,r,u8>> ) ;" 70 SUBJECT:EXITED? TTRUE
   s" the element cannot be exposed by an identity effect" T-LABEL
   s" : C2-INIT-ERASE ( init<p,C2-INIT-SCHEMA:pair> -- C2-INIT-SCHEMA:pair ) ;" 70 SUBJECT:EXITED? TTRUE
   s" raw cells cannot construct initialized storage" T-LABEL
   s" : C2-INIT-FORGE ( -- init<p,C2-INIT-SCHEMA:pair> ) 1 2 ;" 70 SUBJECT:EXITED? TTRUE
   s" a cast cannot erase the initialized lifetime" T-LABEL
   s" CAST: C2-INIT-CAST ( init<p,n> -- n )"
      70 s" cast: 'C2-INIT-CAST': a scoped cast is not two cells on each side" SUBJECT:REFUSED? TTRUE
   s" initialized storage cannot enter a global slot" T-LABEL
   s" TYPED-VARIABLE C2-INIT-GLOBAL init<p,n>" 70 SUBJECT:EXITED? TTRUE ;

;package

T-RESET
C2-INIT-SCHEMA:CHECK
T-REPORT
s" c2-init-schema: ok" type cr
