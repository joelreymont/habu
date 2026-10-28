\ A saved AOT builder for ordinary stripped-image suite children. Resolve it
\ before staging argv or env: the image supplier uses those same tables. This
\ is the image an hb-build child on the candidate selects (tools/hb-build-lib.f
\ HBB-MAKER-IMAGE$), so one settle serves the direct maker runs below and every
\ hb-build child in the same out root.
require src/lib/engine-candidate.f
require tools/hb-build-lib.f
require test/image-grant.f
require test/suite-budget.f

package AOT-BUILDER-ENGINE

public

: PATH$ ( -- ptr u8 n )
   s" aot-builder" IMAGE-GRANT:CHECK
   ENGINE-CANDIDATE:PATH$ BUILD-FIXPOINT:BF-ENGINE!
   SUITE-BUDGET:CHILD-MS HB-BUILD-CLI:HBB-MAKER-IMAGE$ ;

: ENSURE ( -- )
   PATH$ 2drop ;

: STDIN$ ( -- ptr u8 n )
   S\" 1 set-tier\nAOT-BUILDER:OPEN\nAOT-BUILDER:RUN\n" ;

;package
