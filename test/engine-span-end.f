\ engine-span-end.f - the engine's source scanners read only the text they are given.
\
\ Run: bin/hb --load test/engine-span-end.f
\
\ CHECK! and CHECK-DOES! (src/core/checker.f) and the shaker's SHK-TOPLEVEL
\ (src/habu/treeshake.f) scan a caller's text. Their guards tested the bound and
\ the byte with `and`, which evaluates both, so each scan read the byte past the
\ text. Every case copies its text to end on the last byte before an
\ inaccessible page (lib/test/guard-page.f): a read past the end faults, rc 134,
\ where a bounded scan answers what the same text answers anywhere else.
\   - a body whose last token ends at the edge: CHECK-SCAN's token loop;
\   - a signature left open to the edge: CHECK-SCAN's comment loop, then
\     NEXT-SIG-TOK;
\   - a DOES> clause signature ending at the edge: NEXT-SIG-TOK alone;
\   - a program whose one-byte last token ends at the edge: the shaker's OPN2?.

require lib/test.f
require lib/test/guard-page.f
require src/habu/treeshake.f

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

: SHAKE-AT-EDGE ( -- )
   s" dup ." EDGE {: a:ptr u:n :}
   a SHK-A !  u SHK-U !  1 SHAKE? !
   SHK-TOPLEVEL
   0 SHAKE? !
   s" a program whose one-byte last token ends at the edge keeps that token" T-LABEL
   s" ." IN-REACH? TTRUE
   s" dup" IN-REACH? TTRUE ;

: RUN ( -- )
   T-RESET
   BODY-AT-EDGE  SIG-AT-EDGE  CLAUSE-AT-EDGE  SHAKE-AT-EDGE
   T-REPORT ;

RUN
;package
