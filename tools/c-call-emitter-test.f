\ c-call-emitter-test.f - source-shape regression for native C-CALL emitter.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/test/src-shape.f tools/c-call-emitter-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/test/src-shape.f

: CCET-LOAD ( -- )
   s" src/habu/habu2.f" SHAPE:LOAD ;

: CCET-TEST-HELPERS ( -- )
   s" : C-CALL-BRANCH-NO-PROLOGUE ( label -- )" SHAPE:MUST-HAVE
   s" : C-CALL-PROLOGUE-SPAN ( label -- )" SHAPE:MUST-HAVE
   s" : C-CALL-REQUIRE-RET-SLOT ( label -- )" SHAPE:MUST-HAVE
   s" : C-CALL-PLAIN-SPAN ( label -- )" SHAPE:MUST-HAVE
   s" : C-CALL-REJECT-MASKED ( n n label -- )" SHAPE:MUST-HAVE
   s" : C-CALL-REJECT-EXACT ( n label -- )" SHAPE:MUST-HAVE
   s" : C-CALL-REJECT-UNSAFE ( label -- )" SHAPE:MUST-HAVE
   s" : C-CALL-SCAN-SAFE ( label label label -- )" SHAPE:MUST-HAVE
   s" : C-CALL-COPY-INLINE ( label label -- )" SHAPE:MUST-HAVE
   s" : EMIT-CEMITBL ( -- )" SHAPE:MUST-HAVE ;          \ the shared direct-BL call primitive (LCEMITBL)

: CCET-TEST-HELPER-USES ( -- )
   s" C-CALL-BRANCH-NO-PROLOGUE" 2 SHAPE:COUNT=
   s" C-CALL-PROLOGUE-SPAN" 2 SHAPE:COUNT=
   s" C-CALL-PLAIN-SPAN" 2 SHAPE:COUNT=
   s" C-CALL-REJECT-UNSAFE" 2 SHAPE:COUNT=
   s" C-CALL-SCAN-SAFE" 2 SHAPE:COUNT=
   s" C-CALL-COPY-INLINE" 2 SHAPE:COUNT=
   s" LCEMITBL LABEL@ BL," 3 SHAPE:COUNT= ;             \ C-CALL + LP2VEMIT + LP2STORE each emit one direct BL

: CCET-TEST-CALL-BODY ( -- )
   s" lnopro C-CALL-BRANCH-NO-PROLOGUE" SHAPE:MUST-HAVE
   s" lcall C-CALL-PROLOGUE-SPAN" SHAPE:MUST-HAVE
   s" lcall C-CALL-PLAIN-SPAN" SHAPE:MUST-HAVE
   s" lcopy lcall lsbody C-CALL-SCAN-SAFE" SHAPE:MUST-HAVE
   s" linl ldone C-CALL-COPY-INLINE" SHAPE:MUST-HAVE
   s" 9 $94000000 LIT64,  9 9 10 ORR," SHAPE:MUST-HAVE ;   \ EMIT-CEMITBL builds x9 = BL opcode | imm26

: CCET-TEST-REMOVED-DUPLICATION ( -- )
   s" : C-CALL-EMIT-ABSOLUTE" SHAPE:MUST-LACK           \ absolute movz/movk/movk x16 + blr x16 call emitter is gone
   s" : C-CALL-EMIT-MOVZ-X16" SHAPE:MUST-LACK
   s" : C-CALL-EMIT-MOVK-X16" SHAPE:MUST-LACK ;

: CCET-MAIN ( -- )
   T-RESET
   CCET-LOAD
   CCET-TEST-HELPERS
   CCET-TEST-HELPER-USES
   CCET-TEST-CALL-BODY
   CCET-TEST-REMOVED-DUPLICATION
   T-REPORT
   s" c-call-emitter-test: ok" type cr ;

CCET-MAIN
