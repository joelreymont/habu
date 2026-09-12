\ c-call-emitter-test.f - source-shape regression for native C-CALL emitter.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/test/src-shape.f tools/c-call-emitter-test.f
\
\ The words below are counted in the SOURCE TEXT of src/habu/habu2.f, comments
\ included, so a count of 2 means "defined once, named once" only while no comment
\ repeats the name. That is why the prose there says "the inliner's safety scan"
\ and "the inliner's copy loop" instead of spelling those two helpers a third time.
\
\ THE INLINE ARM IS OFF AND TIER 0 ALWAYS CALLS, which is what the emitter's own
\ note in habu2.f says: C-CALL emits one direct BL to the statically known target,
\ and the prologue/span/scan/copy helpers are RETAINED, uncalled, until someone
\ establishes that the scan's instruction set covers everything the IR pipeline can
\ emit into a body. So this file pins two things: the helpers are still there to
\ re-enable, and the emitter does not reach them - re-wiring the inline arm has to
\ change this file too.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/test/src-shape.f

package C-CALL-SHAPE

: SUBJECT ( -- )
   s" src/habu/habu2.f" SHAPE:LOAD ;

: HELPERS ( -- )
   s" : C-CALL-BRANCH-NO-PROLOGUE ( label -- )" SHAPE:MUST-HAVE
   s" : C-CALL-PROLOGUE-SPAN ( label -- )" SHAPE:MUST-HAVE
   s" : C-CALL-REQUIRE-RET-SLOT ( label -- )" SHAPE:MUST-HAVE
   s" : C-CALL-PLAIN-SPAN ( label -- )" SHAPE:MUST-HAVE
   s" : C-CALL-REJECT-MASKED ( n n label -- )" SHAPE:MUST-HAVE
   s" : C-CALL-REJECT-EXACT ( n label -- )" SHAPE:MUST-HAVE
   s" : C-CALL-REJECT-UNSAFE ( label -- )" SHAPE:MUST-HAVE
   s" : C-CALL-SCAN-SAFE ( label label label -- )" SHAPE:MUST-HAVE
   s" : CARRY-SITE ( -- )" SHAPE:MUST-HAVE              \ the copied chain's record, owned by package SNAP-RELOC
   s" : C-CALL-COPY-INLINE ( label label -- )" SHAPE:MUST-HAVE
   s" : EMIT-CEMITBL ( -- )" SHAPE:MUST-HAVE ;          \ the shared direct-BL call primitive (LCEMITBL)

: HELPER-USES ( -- )
   \ one occurrence = the definition and no caller: the three the retired inline
   \ arm used to enter from C-CALL
   s" C-CALL-BRANCH-NO-PROLOGUE" 1 SHAPE:COUNT=
   s" C-CALL-PROLOGUE-SPAN" 1 SHAPE:COUNT=
   s" C-CALL-PLAIN-SPAN" 1 SHAPE:COUNT=
   \ two occurrences = definition + its one site inside another retained helper
   s" C-CALL-REQUIRE-RET-SLOT" 2 SHAPE:COUNT=
   s" C-CALL-REJECT-UNSAFE" 2 SHAPE:COUNT=
   s" CARRY-SITE" 2 SHAPE:COUNT=
   \ the scan and the copy loop are named once by the definition and once by the
   \ note that explains why they are uncalled
   s" C-CALL-SCAN-SAFE" 2 SHAPE:COUNT=
   s" C-CALL-COPY-INLINE" 2 SHAPE:COUNT=
   s" LCEMITBL LABEL@ BL," 3 SHAPE:COUNT= ;             \ C-CALL + LP2VEMIT + LP2STORE each emit one direct BL

: CALL-BODY ( -- )
   \ the whole emitter: one direct BL, no scan and no inline arm
   S\" : C-CALL ( -- )\n   LCEMITBL LABEL@ BL, ;" SHAPE:MUST-HAVE
   s" lnopro C-CALL-BRANCH-NO-PROLOGUE" SHAPE:MUST-LACK
   s" lcall C-CALL-PROLOGUE-SPAN" SHAPE:MUST-LACK
   s" lcall C-CALL-PLAIN-SPAN" SHAPE:MUST-LACK
   s" lcopy lcall lsbody C-CALL-SCAN-SAFE" SHAPE:MUST-LACK
   s" linl ldone C-CALL-COPY-INLINE" SHAPE:MUST-LACK
   s" SNAP-RELOC:CARRY-SITE" SHAPE:MUST-HAVE            \ the retained copy loop still reissues the record it duplicates
   s" 9 $94000000 LIT64,  9 9 10 ORR," SHAPE:MUST-HAVE ;   \ EMIT-CEMITBL builds x9 = BL opcode | imm26

: REMOVED-DUPLICATION ( -- )
   s" : C-CALL-EMIT-ABSOLUTE" SHAPE:MUST-LACK           \ absolute movz/movk/movk x16 + blr x16 call emitter is gone
   s" : C-CALL-EMIT-MOVZ-X16" SHAPE:MUST-LACK
   s" : C-CALL-EMIT-MOVK-X16" SHAPE:MUST-LACK ;

public

: RUN ( -- )
   T-RESET
   SUBJECT
   HELPERS
   HELPER-USES
   CALL-BODY
   REMOVED-DUPLICATION
   T-REPORT
   s" c-call-emitter-test: ok" type cr ;

;package

C-CALL-SHAPE:RUN
