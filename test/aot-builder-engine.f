\ A saved AOT builder for ordinary stripped-image suite children. Resolve it
\ before staging argv or env: the image supplier uses those same tables.
require src/lib/engine-candidate.f
require tools/aot-builder-image.f
require test/image-grant.f
require test/suite-budget.f

package AOT-BUILDER-ENGINE

public

: PATH$ ( -- ptr u8 n )
   s" aot-builder" IMAGE-GRANT:CHECK
   ENGINE-CANDIDATE:PATH$ BUILD-FIXPOINT:BF-ENGINE!
   SUITE-BUDGET:CHILD-MS AOT-BUILDER-IMAGE:PATH$ ;

: ENSURE ( -- )
   PATH$ 2drop ;

: STDIN$ ( -- ptr u8 n )
   S\" 1 set-tier\nAOT-BUILDER:OPEN\nAOT-BUILDER:RUN\n" ;

;package
