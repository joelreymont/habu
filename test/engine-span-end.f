\ engine-span-end.f - the engine's source scanners read only the text they get.
\ CHECK! (src/core/checker.f) scans a caller's text; a guard
\ joining the bound and byte tests with `and` evaluates both and reads past the
\ text. Each case ends its text on the last byte before an inaccessible page
\ (src/lib/test/guard-page.f): a read past the end faults (the memory-fault
\ throw on arm64, the dump and the signal on x86-64), where a bounded scan
\ answers as the text does anywhere.

require src/lib/test.f
require src/lib/test/guard-page.f

package ENGINE-SPAN-END-TEST
private

\ A copy of the text whose last byte is the last readable one.
: EDGE ( str -- str ) {: t:str :}
   t STR:LEN 32 GUARD-PAGE:TAIL {: a:^ :}
   t STR:DATA a t STR:LEN BYTE-COPY
   a t STR:LEN >str ;

\ A body whose last token ends at the edge: CHECK-SCAN's token loop.
: BODY-AT-EDGE ( -- )
   s" a body whose last token ends at the edge certifies" T-LABEL
   s" SPAN-END-BODY ( n -- n ) 1 +" EDGE CHECK! -1 T= ;

\ A signature left open to the edge: CHECK-SCAN's comment loop, then
\ NEXT-SIG-TOK.
: SIG-AT-EDGE ( -- )
   s" a signature left open to the edge certifies" T-LABEL
   s" SPAN-END-SIG ( n -- n" EDGE CHECK! -1 T= ;

: RUN ( -- )
   T-RESET
   BODY-AT-EDGE  SIG-AT-EDGE
   T-REPORT ;

RUN
;package
