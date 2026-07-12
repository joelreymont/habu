\ gate-build-size.f - committed size ratchet for the Habu-under-test candidate.
\
\ Machine-readable baseline manifest, mirroring the TRUSTED.md pattern: the
\ committed rows below are the contract, and the owning gate validates the
\ fresh artifact against them. One row per build target. The candidate build
\ and validate slices (test/gate-engine-lib.f) fail on any candidate larger
\ than its row, so growth needs a deliberate same-commit row bump here.
\ A smaller candidate reports STALE-BASELINE so the shrinking commit lowers
\ the row. A zero row is an unmeasured target and fails closed with the
\ measured size to commit.
\
\ Load after test/gate-common.f.

\ 2026-07-06 macOS bump 115831 -> 132343: TFAM 12 slice 3b pass-2 width-aware
\ transport lowering (habu2.f EM-COMPILE-P2WIDE dispatch, per-op width queries,
\ LP2COPY/LP2ROT/LP2RS emit helpers, width-aware locals carve/reference, and
\ the publish-path recompile trigger). The Linux row still holds the pre-3b
\ value; the next Linux lane run fails closed with the measured size to commit.
\ 2026-07-09 macOS bump 132343 -> 148855: TFAM 10 slice 3a native MATCH lowering
\ (habu2.f J-MATCH + EM-ADT-MATCH-FAM/VAR/OF + EM-MATCH-SEMI + the C-DIE-BAD-TAG
\ inline invalid-tag die, plus the CMBK/CMFR match-frame state and the tfl-match-
\ fam?/tfam-name$/bad-tag kwdata). Page-granular growth (same delta as slice 3b).
\ 2026-07-10 MERGE macOS bump 148855 -> 148855: type-families campaign merged into
\ fable. The merged engine = TFAM slice-3a/3b lowering (above) plus fable's set-check
\ checker-xt validation, BCHECKFETCH, undefined-word EVAL recovery (code 70), and the
\ engine-id/underflow-guard/boot-pin work; those fable fixes fit within the existing
\ 148855-byte page floor, so the macOS ratchet is unchanged on the merged fixpoint.
\ 2026-07-12 macOS measurement remains 148855 after the staged `;package`
\ migration removed the legacy keyword data/dispatch entry.
148855 constant GB-SIZE-BASELINE-MACOS
102592 constant GB-SIZE-BASELINE-LINUX   \ fable re-measure 2026-07-04 (Orin, habu-re-measure-set);
                                         \ STALE for the merged engine (adds the TFAM +1269-line
                                         \ lowering) — the next Linux lane run fails closed with the
                                         \ measured merged size to commit.

0 constant GB-SIZE-OK
1 constant GB-SIZE-GROWN
2 constant GB-SIZE-SHRUNK
3 constant GB-SIZE-MISSING

: GB-SIZE-BASELINE ( -- n )
   HB-TARGET-MACOS? if GB-SIZE-BASELINE-MACOS exit then
   HB-TARGET-LINUX? if GB-SIZE-BASELINE-LINUX exit then
   0 ;

: GB-SIZE-CLASS ( n n -- n ) {: sz:n base:n :}
   base 0= if GB-SIZE-MISSING exit then
   sz base > if GB-SIZE-GROWN exit then
   sz base < if GB-SIZE-SHRUNK exit then
   GB-SIZE-OK ;

: GB-SIZE-PAIR. ( n n -- ) {: sz:n base:n :}
   s" candidate " type sz .
   s" baseline " type base . ;

: GB-SIZE-GROWN-FAIL ( n n -- )
   GB-SIZE-PAIR. cr
   s" candidate size ratchet: grew past test/gate-build-size.f baseline" GE-FAIL ;

: GB-SIZE-STALE-FAIL ( n n -- )
   s" STALE-BASELINE " type
   GB-SIZE-PAIR. cr
   s" candidate size ratchet: shrank below baseline - lower the test/gate-build-size.f row in this commit" GE-FAIL ;

: GB-SIZE-MISSING-FAIL ( n n -- )
   GB-SIZE-PAIR. cr
   s" candidate size ratchet: no baseline row for this target - commit the measured size to test/gate-build-size.f" GE-FAIL ;

\ Pure class->action mapping so the wiring itself is testable: 0 = pass,
\ nonzero = the failing class. Every non-OK class must map to itself.
: GB-SIZE-ACTION ( n -- n ) {: class:n :}
   class GB-SIZE-OK = if GB-SIZE-OK exit then
   class ;

: GB-SIZE-ENFORCE ( n n -- ) {: sz:n base:n :}
   sz base GB-SIZE-CLASS GB-SIZE-ACTION
   case
      GB-SIZE-GROWN of sz base GB-SIZE-GROWN-FAIL endof
      GB-SIZE-SHRUNK of sz base GB-SIZE-STALE-FAIL endof
      GB-SIZE-MISSING of sz base GB-SIZE-MISSING-FAIL endof
   endcase ;

: GB-SIZE-CLASS-EXPECT ( n n n -- ) {: sz:n base:n want:n :}
   sz base GB-SIZE-CLASS want <> if
      s" candidate size ratchet classifier" GE-FAIL
   then ;

: GB-SIZE-ACTION-EXPECT ( n n -- ) {: class:n want:n :}
   class GB-SIZE-ACTION want <> if
      s" candidate size ratchet action wiring" GE-FAIL
   then ;

: GB-SIZE-SELF-CHECK ( -- )
   7 7 GB-SIZE-OK GB-SIZE-CLASS-EXPECT
   8 7 GB-SIZE-GROWN GB-SIZE-CLASS-EXPECT
   6 7 GB-SIZE-SHRUNK GB-SIZE-CLASS-EXPECT
   7 0 GB-SIZE-MISSING GB-SIZE-CLASS-EXPECT
   GB-SIZE-OK GB-SIZE-OK GB-SIZE-ACTION-EXPECT
   GB-SIZE-GROWN GB-SIZE-GROWN GB-SIZE-ACTION-EXPECT
   GB-SIZE-SHRUNK GB-SIZE-SHRUNK GB-SIZE-ACTION-EXPECT
   GB-SIZE-MISSING GB-SIZE-MISSING GB-SIZE-ACTION-EXPECT ;

: GB-SIZE-RATCHET ( ptr u8 n -- )
   GB-SIZE-SELF-CHECK
   FILE-SIZE GB-SIZE-BASELINE GB-SIZE-ENFORCE ;
