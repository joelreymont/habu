\ Generated initialized-field words keep their receiver and value boundaries
\ through ordinary source evaluation.
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/test/subject.f
require src/lib/test/ending.f
require test/c2-init-accessors.f
require src/lib/process.f

package C2-INIT-ACCESSOR-REJECTIONS
private

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot
variable ERR-U

: REJECTED? ( str n -- bool )
   {: src:str expected:n :}
   src OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   {: outu:len erru:len oc :}
   erru LEN>N ERR-U !
   oc MATCH PROCESS:ending
      exited OF expected = ENDOF
      signaled OF drop false ENDOF
      timeout OF
         src OUT outu LEN>N >str ERR erru LEN>N >str T-TIMED-OUT
      ENDOF
   ;MATCH ;

public

: RUN ( -- )
   T-RESET
   \ A declaration's rejection is thrown inside its transaction and rendered by
   \ the runner that wraps it before the code is rethrown, so it exits 70.
   s" an open-width field cannot derive initialized accessors" T-LABEL
   s" package C2IA STRUCTURE badwidth<a> DERIVE init FIELD value a FIELD tail n ;STRUCTURE ;package"
      70 REJECTED? TTRUE
   s" invalid width reports the fixed-cell requirement" T-LABEL
   ERR ERR-U @ >str s" canonical fixed-cell record" STR:CONTAINS? TTRUE
   s" an accessor name a private word holds is rejected" T-LABEL
   s" package C2IA private : PO-A@ ( -- n ) 1 ; STRUCTURE po OPAQUE DERIVE init FIELD a n FIELD b n ;STRUCTURE ;package"
      70 REJECTED? TTRUE
   s" a getter requires its own initialized record" T-LABEL
   s" package C2IA : BAD ( mut-view<a,b,c,init<d,pair>> -- mut-view<a,b,c,init<d,pair>> n ) C2IA-SHELF:COUNT@ ; ;package"
      70 REJECTED? TTRUE
   s" a scoped field setter keeps the source scopes" T-LABEL
   s" package C2IA : BAD ( mut-view<a,b,c,init<d,shelf<a,b>>> read-view<c,d,u8> -- mut-view<a,b,c,init<d,shelf<a,b>>> ) C2IA-SHELF:SOURCE! ; ;package"
      70 REJECTED? TTRUE
   s" a getter cannot return a raw pointer" T-LABEL
   s" package C2IA : BAD ( mut-view<a,b,c,init<d,shelf<a,b>>> -- mut-view<a,b,c,init<d,shelf<a,b>>> ^ ) C2IA-SHELF:SOURCE@ ; ;package"
      70 REJECTED? TTRUE
   T-REPORT
   s" c2-init-accessor-rejections: ok" type cr ;

;package

C2-INIT-ACCESSOR-REJECTIONS:RUN
