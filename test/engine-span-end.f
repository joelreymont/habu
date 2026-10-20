\ engine-span-end.f - the engine's source scanners read only the text they are given.
\
\ Run: bin/hb --load test/engine-span-end.f
\
\ CHECK! and CHECK-DOES! (src/core/checker.f) scan a caller's text. Their
\ guards tested the bound and the byte with `and`, which evaluates both, so
\ each scan read the byte past the text. Every case copies its text to end on
\ the last byte before an
\ inaccessible page (src/lib/test/guard-page.f): a read past the end faults, rc 134,
\ where a bounded scan answers what the same text answers anywhere else.
\   - a body whose last token ends at the edge: CHECK-SCAN's token loop;
\   - a signature left open to the edge: CHECK-SCAN's comment loop, then
\     NEXT-SIG-TOK;
\   - a DOES> clause signature ending at the edge: NEXT-SIG-TOK alone.

require src/lib/test.f
require src/lib/test/guard-page.f

package ENGINE-SPAN-END-TEST
private

\ A copy of the text whose last byte is the last readable one.
: EDGE ( ptr u8 n -- ptr u8 n ) {: t:ptr tu:n :}
   tu 32 GUARD-PAGE:TAIL {: a:ptr :}
   t a tu BYTE-COPY
   a tu ;

: BODY-AT-EDGE ( -- )
   s" a body whose last token ends at the edge certifies" T-LABEL
   s" SPAN-END-BODY ( n -- n ) 1 +" EDGE CHECK! -1 T= ;

: SIG-AT-EDGE ( -- )
   s" a signature left open to the edge certifies" T-LABEL
   s" SPAN-END-SIG ( n -- n" EDGE CHECK! -1 T= ;

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
