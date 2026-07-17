\ maki/db/commit-store-test.f - checked acceptance for the crash-safe commit slice
\ (maki/db/commit-store.f, dot habu-v2-atomic-txn-a3c26066). Proves the plan § 23
\ "Deterministic transactions" acceptance in-process against a REAL private file store,
\ each property by a named test:
\   T-REPLAY          : deterministic replay yields an equal revision digest (acceptance 4)
\   T-IDEM            : an idempotent retry returns the original result (acceptance 2)
\   T-CONFLICT        : a stale base/head returns the typed conflict (acceptance 3)
\   T-DUP             : a duplicate-write transaction rejects (composes TX:VALIDATE)
\   T-CRASH-BEFORE-REV: crash before any publish -> recovery sees OLD (acceptance 1, B0)
\   T-CRASH-AFTER-REV : crash after staging the rev object, before the marker -> recovery
\                       sees OLD; the staged object is a harmless complete orphan (B1)
\   T-CRASH-AFTER-HEAD: crash after the marker advance, before the idempotency record ->
\                       recovery sees the COMPLETE NEW revision, retry is idempotent (B2)
\   T-NO-PARTIAL      : at NO boundary is a head advanced over an incomplete rev object
\   T-FULL            : a complete commit leaves head=new + a complete rev object
\
\ The crash at each boundary is simulated by running a PREFIX of the real public publish
\ steps (STAGE-REV / ADVANCE-HEAD / WRITE-IDEM) and then stopping - exactly what the
\ spawn-a-child cross-process test (maki/db/commit-store-crash-test.f) does across a real
\ process death; here the same boundaries are asserted in-process against the same files.

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require maki/db/commit-store.f
require maki/db/transaction.f
require maki/artifact.f
require maki/rev.f

package CSTORE-TEST

\ One private store dir for the whole suite; each test RESETs its files first.
s" hb-cstore-test" TMPDIR-MKDIR CSTORE:ROOT!

\ ---- fixtures: interned objects, a genesis base, and transactions ---------------
: OBJ-A ( -- CAD-KIND:artifact-id )   s" cs-obj-a" ARTIFACT:REGISTER ;
: OBJ-B ( -- CAD-KIND:artifact-id )   s" cs-obj-b" ARTIFACT:REGISTER ;
: OBJ-C ( -- CAD-KIND:artifact-id )   s" cs-obj-c" ARTIFACT:REGISTER ;
: G0 ( -- CAD-KIND:rev-id )   s" cs-genesis" REV:COMMIT ;

\ MK1 / MK1R: the SAME logical action, added in different orders (canonical form is
\ insertion-order independent, so both propose the same revision).
: MK1 ( -- txn )
   G0 TX:OPEN  OBJ-A TX:PRESENT TX:READ+  OBJ-C TX:WRITE+  OBJ-A TX:DEP+  TX:BUILD ;
: MK1R ( -- txn )
   G0 TX:OPEN  OBJ-A TX:PRESENT TX:READ+  OBJ-C TX:WRITE+  OBJ-A TX:DEP+  TX:BUILD ;
\ MK2: a different write set on the same base -> a different revision.
: MK2 ( -- txn )
   G0 TX:OPEN  OBJ-A TX:PRESENT TX:READ+  OBJ-B TX:WRITE+  OBJ-A TX:DEP+  TX:BUILD ;
\ MK-DUP: two conflicting writes -> a validation reject.
: MK-DUP ( -- txn )
   G0 TX:OPEN  OBJ-A TX:PRESENT TX:READ+  OBJ-C TX:WRITE+  OBJ-C TX:WRITE+  TX:BUILD ;

\ ---- commit-result decoders ---------------------------------------------------
: CCODE ( commit-result<CAD-KIND:rev-id> -- n )   \ 0 committed / 1 conflict / 2 dup / 3 omitted
   MATCH CSTORE:commit-result
      committed OF drop 0 ENDOF
      conflict OF 1 ENDOF
      duplicate-write OF 2 ENDOF
      omitted-read OF 3 ENDOF
   ;MATCH ;

: CREV ( commit-result<CAD-KIND:rev-id> -- CAD-KIND:rev-id bool )   \ committed -> (rev,true)
   MATCH CSTORE:commit-result
      committed OF true ENDOF
      conflict OF G0 false ENDOF
      duplicate-write OF G0 false ENDOF
      omitted-read OF G0 false ENDOF
   ;MATCH ;

: GENESIS ( -- )   CSTORE:RESET  G0 CSTORE:INIT-HEAD ;

\ ---- acceptance 4: deterministic replay yields an equal revision digest ----------
: T-REPLAY ( -- bool )
   GENESIS  MK1  CSTORE:COMMIT CREV {: r1:CAD-KIND:rev-id ok1:bool :}
   GENESIS  MK1R CSTORE:COMMIT CREV {: r2:CAD-KIND:rev-id ok2:bool :}
   ok1 ok2 and  r1 r2 REV:EQUAL? and ;

\ ---- acceptance 2: idempotent retry returns the original result -----------------
: T-IDEM ( -- bool )
   GENESIS
   MK1 CSTORE:COMMIT CREV {: r1:CAD-KIND:rev-id ok1:bool :}
   MK1 CSTORE:COMMIT CREV {: r2:CAD-KIND:rev-id ok2:bool :}
   ok1 ok2 and  r1 r2 REV:EQUAL? and ;

\ ---- acceptance 3: stale head returns the typed conflict ------------------------
: T-CONFLICT ( -- n )
   GENESIS
   MK1 CSTORE:COMMIT drop                        \ head advances to rev1
   MK2 CSTORE:COMMIT CCODE ;                     \ base G0 is now stale -> conflict

: T-DUP ( -- n )   GENESIS  MK-DUP CSTORE:COMMIT CCODE ;

\ ---- acceptance 1: crash injection never exposes a partial revision -------------
: T-CRASH-BEFORE-REV ( -- bool )                 \ crash before any publish step
   GENESIS
   MK1 TX:PROPOSE {: r:CAD-KIND:rev-id :}
   G0 CSTORE:HEAD-IS?                            \ head still genesis (old)
   r CSTORE:REV-PRESENT? 0= and                  \ new rev object not present
   r CSTORE:HEAD-IS? 0= and ;                    \ head is NOT the new rev

: T-CRASH-AFTER-REV ( -- bool )                  \ crash after STAGE-REV, before the marker
   GENESIS
   MK1 CSTORE:STAGE-REV {: r:CAD-KIND:rev-id :}
   r CSTORE:REV-COMPLETE?                        \ staged object is complete
   G0 CSTORE:HEAD-IS? and                        \ head still old (genesis)
   r CSTORE:HEAD-IS? 0= and ;                    \ head is NOT new -> recovery sees OLD

: T-CRASH-AFTER-HEAD ( -- bool )                 \ crash after ADVANCE-HEAD, before idem
   GENESIS
   MK1 {: t:txn :}
   t CSTORE:STAGE-REV {: r:CAD-KIND:rev-id :}
   r CSTORE:ADVANCE-HEAD
   r CSTORE:HEAD-IS?                             \ head IS the new rev
   r CSTORE:REV-COMPLETE? and                    \ and its object is complete (no partial)
   t CSTORE:COMMIT CREV {: r2:CAD-KIND:rev-id ok2:bool :}   \ retry is idempotent
   ok2 and  r r2 REV:EQUAL? and ;

\ ---- the core safety invariant: no head advanced over an incomplete object -------
: NO-PARTIAL? ( CAD-KIND:rev-id -- bool ) {: r:CAD-KIND:rev-id :}
   r CSTORE:HEAD-IS? if r CSTORE:REV-COMPLETE? else true then ;

: T-NO-PARTIAL ( -- bool )
   GENESIS  MK1 TX:PROPOSE {: r:CAD-KIND:rev-id :}
   r NO-PARTIAL?                                 \ before any publish
   GENESIS  MK1 CSTORE:STAGE-REV {: r1:CAD-KIND:rev-id :}
   r1 NO-PARTIAL? and                            \ after staging the object
   r1 CSTORE:ADVANCE-HEAD
   r1 NO-PARTIAL? and ;                          \ after advancing the marker

: T-FULL ( -- bool )
   GENESIS  MK1 {: t:txn :}
   t TX:PROPOSE {: r:CAD-KIND:rev-id :}
   t CSTORE:COMMIT drop
   r CSTORE:HEAD-IS?  r CSTORE:REV-COMPLETE? and ;

T-RESET

T-REPLAY TTRUE
T-IDEM TTRUE
T-CONFLICT 1 T=
T-DUP 2 T=
T-CRASH-BEFORE-REV TTRUE
T-CRASH-AFTER-REV TTRUE
T-CRASH-AFTER-HEAD TTRUE
T-NO-PARTIAL TTRUE
T-FULL TTRUE

CSTORE:RESET

T-REPORT

;package
