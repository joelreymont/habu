\ prim-row-test.f - verify-source reads a row's header and body by the row rule.

require src/lib/test.f
require src/habu/verify-source.f

package PRIM-ROW-TEST
private

0 constant LOADED               \ what `catch` answers for a source that verified

\ A REF operand is one raw token, a string opener or a closer included, so the
\ row runs to its last closer.
: REF-OPERAND ( -- )
   s\" EPRIM: FOO REF s\" REF EPRIM; REF : EPRIM;" VERIFY:SOURCE-BUF-IN-SCOPE ;

: PARSER-BODY ( -- )
   s" EPRIM: FOO postpone EPRIM; PE-N EPRIM;" VERIFY:SOURCE-BUF-IN-SCOPE ;

: ROW-CASE ( -- )
   s" a REF operand is one raw token, a string opener or a closer included"
   T-LABEL
   [: REF-OPERAND ;] LOADED TTHROWSQ

   s" a parsing word in a row body makes the row malformed" T-LABEL
   [: PARSER-BODY ;] VERIFY:E-MALFORMED-REGISTRY-ROW TTHROWSQ

   s" a closer where a row's name stands makes the row malformed" T-LABEL
   [: s" EPRIM: EPRIM; PE-N EPRIM;" VERIFY:SOURCE-BUF-IN-SCOPE ;]
   VERIFY:E-MALFORMED-REGISTRY-ROW TTHROWSQ

   s" an opener where a row's package stands makes the row malformed" T-LABEL
   [: s" EPPRIM: EPRIM: FOO PE-N ECLOSE-PRIVATE" VERIFY:SOURCE-BUF-IN-SCOPE ;]
   VERIFY:E-MALFORMED-REGISTRY-ROW TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   ROW-CASE
   T-REPORT ;

;package

PRIM-ROW-TEST:RUN
