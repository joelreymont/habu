\ A saved AOT maker for ordinary stripped-image suite children. Resolve it
\ before staging argv or env: the image supplier uses those same tables.
require src/lib/engine-candidate.f
require tools/aot-maker-image.f
require test/image-grant.f
require test/suite-budget.f

package AOT-MAKER-ENGINE

public

: PATH$ ( -- ptr u8 n )
   s" aot-maker" IMAGE-GRANT:CHECK
   ENGINE-CANDIDATE:PATH$ BUILD-FIXPOINT:BF-ENGINE!
   SUITE-BUDGET:CHILD-MS AOT-MAKER-IMAGE:PATH$ ;

: ENSURE ( -- )
   PATH$ 2drop ;

: STDIN$ ( -- ptr u8 n )
   S\" 1 set-tier\nAOT-MAKER:OPEN\nAOT-MAKER:RUN\n" ;

;package
