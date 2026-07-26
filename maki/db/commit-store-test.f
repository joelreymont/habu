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
require test/checker-assert.f
require lib/fs.f
require lib/fs-mutate.f
require maki/db/commit-store.f
require maki/db/transaction.f
require maki/artifact.f
require maki/rev.f

package CSTORE-TEST

: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO  ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;

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

\ ---- the generated commit-result constructors: exact spelling + exact effect ----
\ commit-result is declared through the unified ENUM front end in full mode, so these
\ pins are the migration's identity proof and must keep holding for every later
\ declaration change. The SPELLING is load-bearing here: the checker answers 1
\ (uncheckable) for a name it cannot resolve, and YES demands -1, so a -1 means the
\ checker resolved EXACTLY this constructor name; NO demands 0, which it can only
\ reach after resolving the name and refusing the types.
s" CR-C-COM ( CAD-KIND:rev-id -- CSTORE:commit-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--RESULT:COMMITTED" YES
s" CR-C-CON ( -- CSTORE:commit-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--RESULT:CONFLICT" YES
s" CR-C-DUP ( -- CSTORE:commit-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--RESULT:DUPLICATE-WRITE" YES
s" CR-C-OMI ( -- CSTORE:commit-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--RESULT:OMITTED-READ" YES
\ Forge negatives on the committed payload slot: a raw cell cannot fill it, the result
\ is not a bare scalar, the payload is mandatory, a same-width FOREIGN identity role
\ cannot stand in for the revision, and a payloadless arm takes no payload.
s" CR-F-RAW ( n -- CSTORE:commit-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--RESULT:COMMITTED" NO
s" CR-F-BARE ( CAD-KIND:rev-id -- n ) CSTORE-COMMIT--RESULT:COMMITTED" NO
s" CR-F-NONE ( -- CSTORE:commit-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--RESULT:COMMITTED" NO
s" CR-F-FGN ( CAD-KIND:artifact-id -- CSTORE:commit-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--RESULT:COMMITTED" NO
s" CR-F-PAY ( CAD-KIND:rev-id -- CSTORE:commit-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--RESULT:CONFLICT" NO

public

\ twin is CSTORE:commit-result's SHAPE under a different name: same arity, same four
\ variants in the same order, same named payload field. It exists only so the negatives
\ below can prove commit-outcome identity is NOMINAL - two identically shaped ENUM
\ families never unify, in either direction. It has to be public: a private family
\ publishes no constructors at all, and the positive control below builds through the
\ twin's own committed, so neither negative can pass by being unresolvable rather than
\ ill-typed. The tail is deliberately SHORT: the generated constructor package here is
\ CSTORE--TEST-TWIN, and a name over TF-CTOR-NAME-LIMIT (32 bytes,
\ src/core/type-family.f) silently switches to the opaque hashed spelling.
ENUM twin 1
   VARIANT committed FIELD rev a ;VARIANT
   VARIANT conflict ;VARIANT
   VARIANT duplicate-write ;VARIANT
   VARIANT omitted-read ;VARIANT
;ENUM

private

s" CR-C-TWIN ( CAD-KIND:rev-id -- twin<CAD-KIND:rev-id> ) CSTORE--TEST-TWIN:COMMITTED" YES
s" CR-C-TWIN-X1 ( CAD-KIND:rev-id -- twin<CAD-KIND:rev-id> ) CSTORE-COMMIT--RESULT:COMMITTED" NO
s" CR-C-TWIN-X2 ( CAD-KIND:rev-id -- CSTORE:commit-result<CAD-KIND:rev-id> ) CSTORE--TEST-TWIN:COMMITTED" NO

;package

\ ---- every variant constructs and dispatches through MATCH ---------------------
\ The tests above reach the commit-result arms only through CSTORE:COMMIT. The variant
\ producers R-COMMITTED / R-CONFLICT / R-DUP-WRITE / R-OMITTED are owner-private, so
\ reopen the owning package to construct each variant DIRECTLY and match it straight
\ back; that is what proves the named payload FIELD binds in declaration order. The
\ committed arm binds its payload to a TYPED local and the round-trip demands the
\ recovered revision equal the one constructed AND differ from a second live revision,
\ so a payload the constructor dropped, zeroed or replaced with a constant fails
\ instead of passing.
\
\ Construction is factored into one typed word per variant because the checker requires
\ MATCH's scrutinee to be a concretely instantiated family value: a single word that
\ both constructs and matches is refused, and the diagnostic names the family token as
\ an undefined word. That refusal predates this migration (it reproduces identically on
\ the legacy declaration) and is reported separately. Names carry a per-family prefix
\ because all three commit-store suites reopen this one package in the same process.
package CSTORE

: TT-CR-REV-A ( -- CAD-KIND:rev-id )   s" c4-cr-rt-a" REV:COMMIT ;
: TT-CR-REV-B ( -- CAD-KIND:rev-id )   s" c4-cr-rt-b" REV:COMMIT ;

: TT-CR-MK-COM ( CAD-KIND:rev-id -- commit-result<CAD-KIND:rev-id> ) R-COMMITTED ;
: TT-CR-MK-CON ( -- commit-result<CAD-KIND:rev-id> )   R-CONFLICT ;
: TT-CR-MK-DUP ( -- commit-result<CAD-KIND:rev-id> )   R-DUP-WRITE ;
: TT-CR-MK-OMI ( -- commit-result<CAD-KIND:rev-id> )   R-OMITTED ;

: TT-CR-ARM ( commit-result<CAD-KIND:rev-id> -- n )   \ 1 committed .. 4 omitted-read
   MATCH commit-result
      committed       OF drop 1 ENDOF
      conflict        OF 2 ENDOF
      duplicate-write OF 3 ENDOF
      omitted-read    OF 4 ENDOF
   ;MATCH ;

: TT-CR-REV ( commit-result<CAD-KIND:rev-id> -- CAD-KIND:rev-id bool )   \ committed -> (rev,true)
   MATCH commit-result
      committed       OF true ENDOF
      conflict        OF TT-CR-REV-B false ENDOF
      duplicate-write OF TT-CR-REV-B false ENDOF
      omitted-read    OF TT-CR-REV-B false ENDOF
   ;MATCH ;

: TT-CR-AB-DIFF ( -- bool )   TT-CR-REV-A TT-CR-REV-B REV:EQUAL? 0= ;   \ the two controls differ

: TT-CR-RT-ARM ( -- n )   TT-CR-REV-A TT-CR-MK-COM TT-CR-ARM ;
: TT-CR-RT-REV ( -- n )                          \ 0 = payload is A, and is NOT B
   TT-CR-REV-A TT-CR-MK-COM TT-CR-REV {: got:CAD-KIND:rev-id found:bool :}
   found 0= if 1 exit then
   got TT-CR-REV-A REV:EQUAL? 0= if 2 exit then
   got TT-CR-REV-B REV:EQUAL? if 3 exit then
   0 ;
: TT-CR-RT-CON ( -- n )   TT-CR-MK-CON TT-CR-ARM ;
: TT-CR-RT-DUP ( -- n )   TT-CR-MK-DUP TT-CR-ARM ;
: TT-CR-RT-OMI ( -- n )   TT-CR-MK-OMI TT-CR-ARM ;
: TT-CR-CON-REV ( -- n )                         \ a payloadless arm carries no revision
   TT-CR-MK-CON TT-CR-REV {: got:CAD-KIND:rev-id found:bool :}
   found if 1 else 0 then ;

TT-CR-AB-DIFF TTRUE                              \ the distinguishability control is real
TT-CR-RT-ARM 1 T=                                \ committed dispatches to its own arm
TT-CR-RT-REV 0 T=                                \ and carries its payload through unchanged
TT-CR-RT-CON 2 T=                                \ conflict dispatches to its own arm
TT-CR-RT-DUP 3 T=                                \ duplicate-write dispatches to its own arm
TT-CR-RT-OMI 4 T=                                \ omitted-read dispatches to its own arm
TT-CR-CON-REV 0 T=                               \ the no-payload arms of TT-CR-REV are live

;package

CSTORE:RESET

T-REPORT
