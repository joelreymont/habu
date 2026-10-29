\ engine-span-end.f - the engine's source scanners read only the text they get.
\ CHECK! and CHECK-DOES! (src/core/checker.f) scan a caller's text; a guard
\ joining the bound and byte tests with `and` evaluates both and reads past the
\ text. Each case ends its text on the last byte before an inaccessible page
\ (src/lib/test/guard-page.f): a read past the end dies by the signal after the
\ crash handler's dump, where a bounded scan answers as the text does anywhere.

require src/lib/test.f
require src/lib/test/guard-page.f

package ENGINE-SPAN-END-TEST
private

\ A copy of the text whose last byte is the last readable one.
: EDGE ( ptr u8 n -- ptr u8 n ) {: t:ptr tu:n :}
   tu 32 GUARD-PAGE:TAIL {: a:ptr :}
   t a tu BYTE-COPY
   a tu ;

\ A body whose last token ends at the edge: CHECK-SCAN's token loop.
: BODY-AT-EDGE ( -- )
   s" a body whose last token ends at the edge certifies" T-LABEL
   s" SPAN-END-BODY ( n -- n ) 1 +" EDGE CHECK! -1 T= ;

\ A signature left open to the edge: CHECK-SCAN's comment loop, then
\ NEXT-SIG-TOK.
: SIG-AT-EDGE ( -- )
   s" a signature left open to the edge certifies" T-LABEL
   s" SPAN-END-SIG ( n -- n" EDGE CHECK! -1 T= ;

\ A DOES> clause signature ending at the edge: NEXT-SIG-TOK alone.
\ CHECK-DOES! is the engine's own front end, which only a trusted body calls.
TRUSTED: CLAUSE-AT-EDGE ( -- )
   s" a DOES> clause signature that ends at the edge certifies" T-LABEL
   s" drop" s" n -- n" EDGE CHECK-DOES! -1 T= ;

: RUN ( -- )
   T-RESET
   BODY-AT-EDGE  SIG-AT-EDGE  CLAUSE-AT-EDGE
   T-REPORT ;

RUN
;package
