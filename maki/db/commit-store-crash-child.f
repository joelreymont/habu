\ maki/db/commit-store-crash-child.f - the FRESH-PROCESS crash side of the commit-store
\ acceptance (dot habu-v2-atomic-txn-a3c26066). The parent
\ (maki/db/commit-store-crash-test.f) writes a genesis head into a SHARED store dir and
\ spawns a fresh bin/hb that runs RUN-PARTIAL: it performs a PREFIX of the real production
\ publish sequence (STAGE-REV / ADVANCE-HEAD / WRITE-IDEM) against that store and then the
\ process EXITS - an honest crash at that exact durability boundary, no failpoint branch in
\ the production commit path. The parent then reads the store in a fresh image and asserts
\ recovery sees the old revision or the complete new revision, never a partial one.
\
\ Fixtures are content-addressed (identical names -> identical cross-process keys), so the
\ child's proposed revision key equals the parent's: the parent inspects revs/<revhex> and
\ HEAD by the SAME key the child wrote. Package CSCRASH.

require lib/prelude.f
require lib/string.f
require maki/db/commit-store.f
require maki/db/transaction.f
require maki/artifact.f
require maki/rev.f

package CSCRASH
public

: OBJ-A ( -- CAD-KIND:artifact-id )   s" cs-obj-a" ARTIFACT:REGISTER ;
: OBJ-C ( -- CAD-KIND:artifact-id )   s" cs-obj-c" ARTIFACT:REGISTER ;
: G0 ( -- CAD-KIND:rev-id )   s" cs-genesis" REV:COMMIT ;

\ MK1 is the shared transaction both processes build (deterministic canonical form).
: MK1 ( -- txn )
   G0 TX:OPEN  OBJ-A TX:PRESENT TX:READ+  OBJ-C TX:WRITE+  OBJ-A TX:DEP+  TX:BUILD ;

\ RUN-PARTIAL performs `steps` of the real publish against the shared store, then returns -
\ the process exits here, simulating a crash at that boundary:
\   steps 1 : STAGE-REV only            (rev object staged; commit marker NOT advanced)
\   steps 2 : STAGE-REV + ADVANCE-HEAD  (marker advanced; idempotency record NOT written)
\   steps 3 : the full publish (baseline: a clean complete commit)
: RUN-PARTIAL ( ptr u8 n n -- ) {: pa:ptr pu:n steps:n :}
   pa pu CSTORE:ROOT!
   MK1 {: t:txn :}
   t CSTORE:STAGE-REV {: r:CAD-KIND:rev-id :}
   steps 2 >= if r CSTORE:ADVANCE-HEAD then
   steps 3 >= if t r CSTORE:WRITE-IDEM then
   s" CSCRASH-DONE" type ;

;package
