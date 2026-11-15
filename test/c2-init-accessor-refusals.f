\ Generated initialized-field words keep their receiver and value boundaries
\ through ordinary source evaluation.
require src/lib/string.f
require src/lib/test.f
require src/lib/test/subject.f
require src/lib/test/ending.f
require test/c2-init-accessors.f

package C2-INIT-ACCESSOR-REFUSALS
private

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot
variable ERR-U

: REFUSED? ( ^u8 n n -- bool )
   {: src:^ srcu:n expected:n :}
   src srcu OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   {: outu:len erru:len oc :}
   erru LEN>N ERR-U !
   oc MATCH PROCESS:ending
      exited OF expected = ENDOF
      signaled OF drop false ENDOF
      timeout OF src srcu OUT outu LEN>N ERR erru LEN>N T-TIMED-OUT ENDOF
   ;MATCH ;

public

: RUN ( -- )
   T-RESET
   \ A declaration's refusal is thrown inside its transaction and rendered by
   \ the runner that wraps it before the code is rethrown, so it exits 70.
   s" an open-width field cannot derive initialized accessors" T-LABEL
   s" package C2IA STRUCTURE badwidth<a> DERIVE init FIELD value a FIELD tail n ;STRUCTURE ;package"
      70 REFUSED? TTRUE
   s" invalid width reports the fixed-cell requirement" T-LABEL
   ERR ERR-U @ s" canonical fixed-cell record" CONTAINS? TTRUE
   s" an accessor name a private word holds is refused" T-LABEL
   s" package C2IA private : PO-A@ ( -- n ) 1 ; STRUCTURE po OPAQUE DERIVE init FIELD a n FIELD b n ;STRUCTURE ;package"
      70 REFUSED? TTRUE
   s" a getter requires its own initialized record" T-LABEL
   s" package C2IA : BAD ( mut-view<a,b,c,init<d,pair>> -- mut-view<a,b,c,init<d,pair>> n ) C2IA-SHELF:COUNT@ ; ;package"
      70 REFUSED? TTRUE
   s" a scoped field setter keeps the source scopes" T-LABEL
   s" package C2IA : BAD ( mut-view<a,b,c,init<d,shelf<a,b>>> read-view<c,d,u8> -- mut-view<a,b,c,init<d,shelf<a,b>>> ) C2IA-SHELF:SOURCE! ; ;package"
      70 REFUSED? TTRUE
   s" a getter cannot return a raw pointer" T-LABEL
   s" package C2IA : BAD ( mut-view<a,b,c,init<d,shelf<a,b>>> -- mut-view<a,b,c,init<d,shelf<a,b>>> ^ ) C2IA-SHELF:SOURCE@ ; ;package"
      70 REFUSED? TTRUE
   T-REPORT
   s" c2-init-accessor-refusals: ok" type cr ;

;package

C2-INIT-ACCESSOR-REFUSALS:RUN
