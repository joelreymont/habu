\ Generated initialized-field words keep their receiver, value, and private
\ representation boundaries through ordinary source evaluation at both tiers.
require lib/test.f
require lib/test/subject.f
require test/c2-init-accessors.f

package C2-INIT-ACCESSOR-REFUSALS
private

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot
variable ERR-U

: REFUSED? ( ptr u8 n n -- bool ) {: expected:n :}
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF expected = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   >r LEN>N ERR-U ! drop r> ;

public

: RUN ( -- )
   T-RESET
   \ The STRUCTURE refusal is rendered before its code is thrown, so it exits 70;
   \ the PRODUCT one below throws its code with nothing rendered and exits 67.
   s" an open-width field cannot derive initialized accessors" T-LABEL
   s" package C2IA STRUCTURE badwidth 1 DERIVE init FIELD value a FIELD tail n ;STRUCTURE ;package"
      70 REFUSED? TTRUE
   s" invalid width reports the fixed-cell requirement" T-LABEL
   ERR ERR-U @ s" canonical fixed-cell record" CONTAINS? TTRUE
   s" a product with an open-width field cannot derive init" T-LABEL
   s" package C2IA PRODUCT badproduct 1 DERIVE init FIELD value a ;PRODUCT ;package"
      67 REFUSED? TTRUE
   s" a getter requires its own initialized record" T-LABEL
   s" package C2IA : BAD ( mut-view<a,b,c,init<d,pair>> -- mut-view<a,b,c,init<d,pair>> n ) C2IA-SHELF:COUNT@ ; ;package"
      70 REFUSED? TTRUE
   s" a scoped field setter keeps the source scopes" T-LABEL
   s" package C2IA : BAD ( mut-view<a,b,c,init<d,shelf<a,b>>> read-view<c,d,u8> -- mut-view<a,b,c,init<d,shelf<a,b>>> ) C2IA-SHELF:SOURCE! ; ;package"
      70 REFUSED? TTRUE
   s" a getter cannot return a raw pointer" T-LABEL
   s" package C2IA : BAD ( mut-view<a,b,c,init<d,shelf<a,b>>> -- mut-view<a,b,c,init<d,shelf<a,b>>> ptr ) C2IA-SHELF:SOURCE@ ; ;package"
      70 REFUSED? TTRUE
   \ Intel runs native tier one only; the explicit tier-one refusal below
   \ still tests this private helper on that product.
   tier@ 0= if
      s" the private unpack helper is not callable at tier zero" T-LABEL
      s" package C2IA : BAD ( read-view<a,b,u8> -- n n ) SHELF-SOURCE-INIT-UNPACK ; ;package"
         70 REFUSED? TTRUE
   then
   s" the private unpack helper is not callable at tier one" T-LABEL
   s" 1 set-tier package C2IA : BAD ( read-view<a,b,u8> -- n n ) SHELF-SOURCE-INIT-UNPACK ; ;package"
      67 REFUSED? TTRUE
   tier@ 0= if
      s" the private receiver helper is not callable at tier zero" T-LABEL
      s" package C2IA : BAD ( mut-view<a,b,c,init<d,shelf<a,b>>> -- ptr u8 n ) SHELF-INIT-VIEW-UNPACK ; ;package"
         70 REFUSED? TTRUE
   then
   s" the private receiver helper is not callable at tier one" T-LABEL
   s" 1 set-tier package C2IA : BAD ( mut-view<a,b,c,init<d,shelf<a,b>>> -- ptr u8 n ) SHELF-INIT-VIEW-UNPACK ; ;package"
      67 REFUSED? TTRUE
   s" an internal helper cannot be exported at tier zero" T-LABEL
   s" package C2IA public EXPORT SHELF-SOURCE-INIT-UNPACK ;package"
      70 REFUSED? TTRUE
   s" an internal helper cannot be exported at tier one" T-LABEL
   s" 1 set-tier package C2IA public EXPORT SHELF-SOURCE-INIT-UNPACK ;package"
      70 REFUSED? TTRUE
   s" an internal receiver cannot be exported at tier zero" T-LABEL
   s" package C2IA public EXPORT SHELF-INIT-VIEW-UNPACK ;package"
      70 REFUSED? TTRUE
   s" an internal receiver cannot be exported at tier one" T-LABEL
   s" 1 set-tier package C2IA public EXPORT SHELF-INIT-VIEW-UNPACK ;package"
      70 REFUSED? TTRUE
   T-REPORT
   s" c2-init-accessor-refusals: ok" type cr ;

;package

C2-INIT-ACCESSOR-REFUSALS:RUN
