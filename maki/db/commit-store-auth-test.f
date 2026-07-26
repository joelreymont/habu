\ maki/db/commit-store-auth-test.f - checked acceptance for the CAPABILITY + BUDGET gated commit
\ (CSTORE:COMMIT-AUTHORIZED, maki/db/commit-store.f, dot habu-v2-capability-and-0970a96d). Proves
\ the deferred § 23 "capability and resource budgets cover all effects" commit legs (plan:3726)
\ against a REAL private file store, each property by a named test:
\   CA-PLAIN-OK       : a txn with no declared caps/budget commits through the gate (empty ⊆ any)
\   CA-UNAUTH / -HEAD : granted authority ⊉ declared caps -> unauthorized, HEAD unchanged (no publish)
\   CA-OK / -HEAD     : full grant + sufficient ledger -> committed, HEAD advances to the new rev
\   CA-CHARGE-ONCE    : a committed authorized commit charges the ledger exactly once
\   CA-DOUBLE-CHARGE  : an idempotent RETRY of the same txn does NOT charge again (no double charge)
\   CA-RETRY-RESULT   : a retry returns the SAME committed revision (idempotent result)
\   CA-EXHAUST/-DIM   : declared reserve > ledger remaining -> exhausted, naming the dimension
\   CA-EXHAUST-HEAD   : an exhausted commit leaves HEAD unchanged (no partial publish, crash-style)
\   CA-EXHAUST-NOCHARGE / CA-DUP-NOCHARGE : a rejected commit charges nothing
\
\ Fresh package (the maki/db/commit-store-test precedent): MATCH reads CSTORE:auth-result qualified.

require lib/prelude.f
require lib/test.f
require test/checker-assert.f
require lib/fs.f
require maki/db/commit-store.f
require maki/db/transaction.f
require maki/db/capability.f
require maki/db/budget-ledger.f
require maki/db/budget-dim.f
require maki/artifact.f
require maki/rev.f

package CSTORE-AUTH-TEST

: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO  ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;

\ One private store dir for the whole suite; each test RESETs its files first.
s" hb-cstore-auth-test" TMPDIR-MKDIR CSTORE:ROOT!

\ ---- fixtures: interned objects + a genesis base --------------------------------
: OBJ-A ( -- CAD-KIND:artifact-id )   s" ca-obj-a" ARTIFACT:REGISTER ;
: OBJ-C ( -- CAD-KIND:artifact-id )   s" ca-obj-c" ARTIFACT:REGISTER ;
: G0 ( -- CAD-KIND:rev-id )   s" ca-genesis" REV:COMMIT ;
: GENESIS ( -- )   CSTORE:RESET  G0 CSTORE:INIT-HEAD ;

\ A txn declaring capability codes {1,2} (mask 3) and a compute-time reserve of 40.
: MK-CAP-TXN ( -- txn )
   G0 TX:OPEN
   OBJ-A TX:PRESENT TX:READ+  OBJ-C TX:WRITE+  OBJ-A TX:DEP+
   1 TX:CAP+  2 TX:CAP+
   BUDGET-DIM:COMPUTE-TIME BUDGET:DIM>N 40 TX:BUDGET+
   TX:BUILD ;

\ A plain txn (no declared caps/budget) - the empty-authority pass-through.
: MK-PLAIN ( -- txn )
   G0 TX:OPEN  OBJ-A TX:PRESENT TX:READ+  OBJ-C TX:WRITE+  OBJ-A TX:DEP+  TX:BUILD ;

\ A duplicate-write txn WITH sufficient caps/budget: the gate passes, COMMIT rejects it.
: MK-DUP-CAP ( -- txn )
   G0 TX:OPEN
   OBJ-A TX:PRESENT TX:READ+  OBJ-C TX:WRITE+  OBJ-C TX:WRITE+
   1 TX:CAP+  2 TX:CAP+
   BUDGET-DIM:COMPUTE-TIME BUDGET:DIM>N 40 TX:BUDGET+
   TX:BUILD ;

\ Grants: FULL authorizes mask 3 (cap bits 0,1,2) with a 100 compute-time ceiling; WEAK holds only
\ bit 0 (mask 1), so it does NOT authorize the declared {1,2}.
: MK-GRANT-FULL ( -- CAPTOK:grant )
   CAPTOK:RESET CAPTOK:NEW  7 CAPTOK:CAP!  BUDGET-DIM:COMPUTE-TIME 100 CAPTOK:BUDGET!  CAPTOK:ROOT ;
: MK-GRANT-WEAK ( -- CAPTOK:grant )
   CAPTOK:RESET CAPTOK:NEW  1 CAPTOK:CAP!  BUDGET-DIM:COMPUTE-TIME 100 CAPTOK:BUDGET!  CAPTOK:ROOT ;

\ Ledgers: a 100-compute ledger admits the 40 reserve; a 20-compute ledger exhausts it.
: MK-LEDGER ( -- LEDGER:ledger )
   LEDGER:RESET LEDGER:OPEN {: l:LEDGER:ledger :}
   l BUDGET-DIM:COMPUTE-TIME 100 LEDGER:LIMIT!  l ;
: MK-LEDGER-SMALL ( -- LEDGER:ledger )
   LEDGER:RESET LEDGER:OPEN {: l:LEDGER:ledger :}
   l BUDGET-DIM:COMPUTE-TIME 20 LEDGER:LIMIT!  l ;

\ ---- auth-result decoders ------------------------------------------------------
: ACODE ( auth-result<CAD-KIND:rev-id> -- n )   \ 0 committed / 1 conflict / 2 dup / 3 omitted / 4 unauth / 5 exhausted
   MATCH CSTORE:auth-result
      committed       OF drop 0 ENDOF
      conflict        OF 1 ENDOF
      duplicate-write OF 2 ENDOF
      omitted-read    OF 3 ENDOF
      unauthorized    OF 4 ENDOF
      exhausted       OF drop 5 ENDOF
   ;MATCH ;

: AEXDIM ( auth-result<CAD-KIND:rev-id> -- n )   \ exhausted dimension ordinal, else -1
   MATCH CSTORE:auth-result
      committed       OF drop -1 ENDOF
      conflict        OF -1 ENDOF
      duplicate-write OF -1 ENDOF
      omitted-read    OF -1 ENDOF
      unauthorized    OF -1 ENDOF
      exhausted       OF BUDGET:DIM>N ENDOF
   ;MATCH ;

: AREV ( auth-result<CAD-KIND:rev-id> -- CAD-KIND:rev-id bool )   \ committed -> (rev,true)
   MATCH CSTORE:auth-result
      committed       OF true ENDOF
      conflict        OF G0 false ENDOF
      duplicate-write OF G0 false ENDOF
      omitted-read    OF G0 false ENDOF
      unauthorized    OF G0 false ENDOF
      exhausted       OF drop G0 false ENDOF
   ;MATCH ;

\ ---- pass-through + authorization ----------------------------------------------
: CA-PLAIN-OK ( -- n )
   GENESIS  MK-PLAIN MK-GRANT-FULL MK-LEDGER CSTORE:COMMIT-AUTHORIZED ACODE ;
: CA-UNAUTH ( -- n )
   GENESIS  MK-CAP-TXN MK-GRANT-WEAK MK-LEDGER CSTORE:COMMIT-AUTHORIZED ACODE ;
: CA-UNAUTH-HEAD ( -- bool )                     \ HEAD still genesis after an unauthorized commit
   GENESIS  MK-CAP-TXN MK-GRANT-WEAK MK-LEDGER CSTORE:COMMIT-AUTHORIZED drop
   G0 CSTORE:HEAD-IS? ;

\ ---- authorized commit + exactly-once charge -----------------------------------
: CA-OK ( -- n )
   GENESIS  MK-CAP-TXN MK-GRANT-FULL MK-LEDGER CSTORE:COMMIT-AUTHORIZED ACODE ;
: CA-OK-HEAD ( -- bool )
   GENESIS  MK-CAP-TXN {: t:txn :}  t TX:PROPOSE {: r:CAD-KIND:rev-id :}
   t MK-GRANT-FULL MK-LEDGER CSTORE:COMMIT-AUTHORIZED drop
   r CSTORE:HEAD-IS? ;
: CA-CHARGE-ONCE ( -- n )
   GENESIS  MK-GRANT-FULL {: g:CAPTOK:grant :}  MK-LEDGER {: l:LEDGER:ledger :}
   MK-CAP-TXN {: t:txn :}
   t g l CSTORE:COMMIT-AUTHORIZED drop
   l BUDGET-DIM:COMPUTE-TIME LEDGER:REMAINING@ ;

\ ---- idempotent retry: no double charge, same result ---------------------------
: CA-DOUBLE-CHARGE ( -- n )
   GENESIS  MK-GRANT-FULL {: g:CAPTOK:grant :}  MK-LEDGER {: l:LEDGER:ledger :}
   MK-CAP-TXN {: t:txn :}
   t g l CSTORE:COMMIT-AUTHORIZED drop          \ first publish: charge 40 -> remaining 60
   t g l CSTORE:COMMIT-AUTHORIZED drop          \ retry: idempotent, no charge -> remaining 60
   l BUDGET-DIM:COMPUTE-TIME LEDGER:REMAINING@ ;
: CA-RETRY-RESULT ( -- bool )
   GENESIS  MK-GRANT-FULL {: g:CAPTOK:grant :}  MK-LEDGER {: l:LEDGER:ledger :}
   MK-CAP-TXN {: t:txn :}
   t TX:PROPOSE {: r:CAD-KIND:rev-id :}
   t g l CSTORE:COMMIT-AUTHORIZED AREV {: r1:CAD-KIND:rev-id ok1:bool :}
   t g l CSTORE:COMMIT-AUTHORIZED AREV {: r2:CAD-KIND:rev-id ok2:bool :}
   ok1 ok2 and  r1 r2 REV:EQUAL? and  r1 r REV:EQUAL? and ;

\ ---- exhaustion: typed, dimension-named, no partial publish, no charge ----------
: CA-EXHAUST ( -- n )
   GENESIS  MK-CAP-TXN MK-GRANT-FULL MK-LEDGER-SMALL CSTORE:COMMIT-AUTHORIZED ACODE ;
: CA-EXHAUST-DIM ( -- n )
   GENESIS  MK-CAP-TXN MK-GRANT-FULL MK-LEDGER-SMALL CSTORE:COMMIT-AUTHORIZED AEXDIM ;
: CA-EXHAUST-HEAD ( -- bool )                    \ exhausted commit leaves HEAD unchanged (crash-style)
   GENESIS  MK-CAP-TXN MK-GRANT-FULL MK-LEDGER-SMALL CSTORE:COMMIT-AUTHORIZED drop
   G0 CSTORE:HEAD-IS? ;
: CA-EXHAUST-NOCHARGE ( -- n )
   GENESIS  MK-GRANT-FULL {: g:CAPTOK:grant :}  MK-LEDGER-SMALL {: l:LEDGER:ledger :}
   MK-CAP-TXN {: t:txn :}
   t g l CSTORE:COMMIT-AUTHORIZED drop
   l BUDGET-DIM:COMPUTE-TIME LEDGER:REMAINING@ ;

\ ---- a validation-reject (duplicate write) charges nothing ----------------------
: CA-DUP ( -- n )
   GENESIS  MK-DUP-CAP MK-GRANT-FULL MK-LEDGER CSTORE:COMMIT-AUTHORIZED ACODE ;
: CA-DUP-NOCHARGE ( -- n )
   GENESIS  MK-GRANT-FULL {: g:CAPTOK:grant :}  MK-LEDGER {: l:LEDGER:ledger :}
   MK-DUP-CAP {: t:txn :}
   t g l CSTORE:COMMIT-AUTHORIZED drop
   l BUDGET-DIM:COMPUTE-TIME LEDGER:REMAINING@ ;

T-RESET

CA-PLAIN-OK 0 T=

CA-UNAUTH 4 T=
CA-UNAUTH-HEAD TTRUE

CA-OK 0 T=
CA-OK-HEAD TTRUE
CA-CHARGE-ONCE 60 T=

CA-DOUBLE-CHARGE 60 T=
CA-RETRY-RESULT TTRUE

CA-EXHAUST 5 T=
CA-EXHAUST-DIM 0 T=
CA-EXHAUST-HEAD TTRUE
CA-EXHAUST-NOCHARGE 20 T=

CA-DUP 2 T=
CA-DUP-NOCHARGE 100 T=

\ ---- the generated auth-result constructors: exact spelling + exact effect ------
\ auth-result is declared through the unified ENUM front end in full mode, so these pins
\ are the migration's identity proof and must keep holding for every later declaration
\ change. They matter more than the other two families in this file's neighbourhood:
\ maki/db/agent-loop.f MATCHes this family across a package boundary, so a drifted
\ constructor spelling would break a consumer this suite never loads. The SPELLING is
\ load-bearing: the checker answers 1 (uncheckable) for a name it cannot resolve, and YES
\ demands -1, so a -1 means the checker resolved EXACTLY this constructor name; NO demands
\ 0, which it can only reach after resolving the name and refusing the types.
s" AR-C-COM ( CAD-KIND:rev-id -- CSTORE:auth-result<CAD-KIND:rev-id> ) CSTORE-AUTH--RESULT:COMMITTED" YES
s" AR-C-CON ( -- CSTORE:auth-result<CAD-KIND:rev-id> ) CSTORE-AUTH--RESULT:CONFLICT" YES
s" AR-C-DUP ( -- CSTORE:auth-result<CAD-KIND:rev-id> ) CSTORE-AUTH--RESULT:DUPLICATE-WRITE" YES
s" AR-C-OMI ( -- CSTORE:auth-result<CAD-KIND:rev-id> ) CSTORE-AUTH--RESULT:OMITTED-READ" YES
s" AR-C-UNA ( -- CSTORE:auth-result<CAD-KIND:rev-id> ) CSTORE-AUTH--RESULT:UNAUTHORIZED" YES
s" AR-C-EXH ( BUDGET:dim -- CSTORE:auth-result<CAD-KIND:rev-id> ) CSTORE-AUTH--RESULT:EXHAUSTED" YES
\ Forge negatives. The first four are the committed payload slot: a raw cell cannot fill
\ it, the result is not a bare scalar, the payload is mandatory, and a same-width FOREIGN
\ identity role cannot stand in for the revision. The last four are what having TWO named
\ payloads in one family adds: the exhausted dimension is mandatory, it is not a raw cell,
\ and NEITHER payload role can stand in for the other.
s" AR-F-RAW ( n -- CSTORE:auth-result<CAD-KIND:rev-id> ) CSTORE-AUTH--RESULT:COMMITTED" NO
s" AR-F-BARE ( CAD-KIND:rev-id -- n ) CSTORE-AUTH--RESULT:COMMITTED" NO
s" AR-F-NONE ( -- CSTORE:auth-result<CAD-KIND:rev-id> ) CSTORE-AUTH--RESULT:COMMITTED" NO
s" AR-F-FGN ( CAD-KIND:artifact-id -- CSTORE:auth-result<CAD-KIND:rev-id> ) CSTORE-AUTH--RESULT:COMMITTED" NO
s" AR-F-EXH-NONE ( -- CSTORE:auth-result<CAD-KIND:rev-id> ) CSTORE-AUTH--RESULT:EXHAUSTED" NO
s" AR-F-EXH-RAW ( n -- CSTORE:auth-result<CAD-KIND:rev-id> ) CSTORE-AUTH--RESULT:EXHAUSTED" NO
s" AR-F-EXH-AS-REV ( CAD-KIND:rev-id -- CSTORE:auth-result<CAD-KIND:rev-id> ) CSTORE-AUTH--RESULT:EXHAUSTED" NO
s" AR-F-COM-AS-DIM ( BUDGET:dim -- CSTORE:auth-result<CAD-KIND:rev-id> ) CSTORE-AUTH--RESULT:COMMITTED" NO

public

\ twin is CSTORE:auth-result's SHAPE under a different name: same arity, same six
\ variants in the same order, same two named payload fields. It exists only so the
\ negatives below can prove commit-outcome identity is NOMINAL - two identically shaped
\ ENUM families never unify, in either direction. It has to be public: a private family
\ publishes no constructors at all, and the positive control below builds through the
\ twin's own committed, so neither negative can pass by being unresolvable rather than
\ ill-typed. The tail is deliberately SHORT: the generated constructor package here is
\ CSTORE--AUTH--TEST-TWIN, and a name over TF-CTOR-NAME-LIMIT (32 bytes,
\ src/core/type-family.f) silently switches to the opaque hashed spelling.
ENUM twin 1
   VARIANT committed FIELD rev a ;VARIANT
   VARIANT conflict ;VARIANT
   VARIANT duplicate-write ;VARIANT
   VARIANT omitted-read ;VARIANT
   VARIANT unauthorized ;VARIANT
   VARIANT exhausted FIELD dim BUDGET:dim ;VARIANT
;ENUM

private

s" AR-C-TWIN ( CAD-KIND:rev-id -- twin<CAD-KIND:rev-id> ) CSTORE--AUTH--TEST-TWIN:COMMITTED" YES
s" AR-C-TWIN-X1 ( CAD-KIND:rev-id -- twin<CAD-KIND:rev-id> ) CSTORE-AUTH--RESULT:COMMITTED" NO
s" AR-C-TWIN-X2 ( CAD-KIND:rev-id -- CSTORE:auth-result<CAD-KIND:rev-id> ) CSTORE--AUTH--TEST-TWIN:COMMITTED" NO

;package

\ ---- every variant constructs and dispatches through MATCH ---------------------
\ The tests above reach the auth-result arms only through CSTORE:COMMIT-AUTHORIZED. The
\ variant producers A-COMMITTED .. A-EXHAUSTED are owner-private, so reopen the owning
\ package to construct each variant DIRECTLY and match it straight back; that is what
\ proves both named payload FIELDs bind in declaration order. Each payload arm binds to a
\ TYPED local: the committed round-trip demands the recovered revision equal the one
\ constructed AND differ from a second live revision, and the exhausted round-trip uses
\ `retries` (ordinal 4) rather than `compute-time`, because compute-time's ordinal is 0
\ and a dropped or zeroed dimension payload would read back as 0 and pass. The
\ CA-EXHAUST-DIM leg above is exactly that zero-ordinal case, so this is the leg that
\ can see a lost dimension payload.
\
\ Construction is factored into one typed word per variant because the checker requires
\ MATCH's scrutinee to be a concretely instantiated family value: a single word that both
\ constructs and matches is refused, and the diagnostic names the family token as an
\ undefined word. That refusal predates this migration (it reproduces identically on the
\ legacy declaration) and is reported separately. Names carry a per-family prefix because
\ all three commit-store suites reopen this one package in the same process.
package CSTORE

: TT-AR-REV-A ( -- CAD-KIND:rev-id )   s" c4-ar-rt-a" REV:COMMIT ;
: TT-AR-REV-B ( -- CAD-KIND:rev-id )   s" c4-ar-rt-b" REV:COMMIT ;
: TT-AR-DIM ( -- BUDGET:dim )          BUDGET-DIM:RETRIES ;   \ ordinal 4, deliberately non-zero

: TT-AR-MK-COM ( CAD-KIND:rev-id -- auth-result<CAD-KIND:rev-id> ) A-COMMITTED ;
: TT-AR-MK-CON ( -- auth-result<CAD-KIND:rev-id> )   A-CONFLICT ;
: TT-AR-MK-DUP ( -- auth-result<CAD-KIND:rev-id> )   A-DUP-WRITE ;
: TT-AR-MK-OMI ( -- auth-result<CAD-KIND:rev-id> )   A-OMITTED ;
: TT-AR-MK-UNA ( -- auth-result<CAD-KIND:rev-id> )   A-UNAUTHORIZED ;
: TT-AR-MK-EXH ( BUDGET:dim -- auth-result<CAD-KIND:rev-id> ) A-EXHAUSTED ;

: TT-AR-ARM ( auth-result<CAD-KIND:rev-id> -- n )   \ 1 committed .. 6 exhausted
   MATCH auth-result
      committed       OF drop 1 ENDOF
      conflict        OF 2 ENDOF
      duplicate-write OF 3 ENDOF
      omitted-read    OF 4 ENDOF
      unauthorized    OF 5 ENDOF
      exhausted       OF drop 6 ENDOF
   ;MATCH ;

: TT-AR-REV ( auth-result<CAD-KIND:rev-id> -- CAD-KIND:rev-id bool )   \ committed -> (rev,true)
   MATCH auth-result
      committed       OF true ENDOF
      conflict        OF TT-AR-REV-B false ENDOF
      duplicate-write OF TT-AR-REV-B false ENDOF
      omitted-read    OF TT-AR-REV-B false ENDOF
      unauthorized    OF TT-AR-REV-B false ENDOF
      exhausted       OF drop TT-AR-REV-B false ENDOF
   ;MATCH ;

: TT-AR-DIMN ( auth-result<CAD-KIND:rev-id> -- n )   \ exhausted dimension ordinal, else -1
   MATCH auth-result
      committed       OF drop -1 ENDOF
      conflict        OF -1 ENDOF
      duplicate-write OF -1 ENDOF
      omitted-read    OF -1 ENDOF
      unauthorized    OF -1 ENDOF
      exhausted       OF {: d:BUDGET:dim :} d BUDGET:DIM>N ENDOF
   ;MATCH ;

: TT-AR-AB-DIFF ( -- bool )      TT-AR-REV-A TT-AR-REV-B REV:EQUAL? 0= ;
: TT-AR-DIM-NONZERO ( -- bool )  TT-AR-DIM BUDGET:DIM>N 0<> ;

: TT-AR-RT-ARM ( -- n )   TT-AR-REV-A TT-AR-MK-COM TT-AR-ARM ;
: TT-AR-RT-REV ( -- n )                          \ 0 = payload is A, and is NOT B
   TT-AR-REV-A TT-AR-MK-COM TT-AR-REV {: got:CAD-KIND:rev-id found:bool :}
   found 0= if 1 exit then
   got TT-AR-REV-A REV:EQUAL? 0= if 2 exit then
   got TT-AR-REV-B REV:EQUAL? if 3 exit then
   0 ;
: TT-AR-RT-CON ( -- n )   TT-AR-MK-CON TT-AR-ARM ;
: TT-AR-RT-DUP ( -- n )   TT-AR-MK-DUP TT-AR-ARM ;
: TT-AR-RT-OMI ( -- n )   TT-AR-MK-OMI TT-AR-ARM ;
: TT-AR-RT-UNA ( -- n )   TT-AR-MK-UNA TT-AR-ARM ;
: TT-AR-RT-EXH ( -- n )   TT-AR-DIM TT-AR-MK-EXH TT-AR-ARM ;
: TT-AR-RT-DIM ( -- n )   TT-AR-DIM TT-AR-MK-EXH TT-AR-DIMN ;   \ the dimension itself
: TT-AR-CON-DIM ( -- n )  TT-AR-MK-CON TT-AR-DIMN ;             \ payloadless: no dimension
: TT-AR-CON-REV ( -- n )                         \ payloadless: no revision
   TT-AR-MK-CON TT-AR-REV {: got:CAD-KIND:rev-id found:bool :}
   found if 1 else 0 then ;
: TT-AR-EXH-REV ( -- n )                         \ the exhausted arm carries no revision
   TT-AR-DIM TT-AR-MK-EXH TT-AR-REV {: got:CAD-KIND:rev-id found:bool :}
   found if 1 else 0 then ;

TT-AR-AB-DIFF TTRUE                              \ the distinguishability control is real
TT-AR-DIM-NONZERO TTRUE                          \ and the dimension under test is not ordinal 0
TT-AR-RT-ARM 1 T=                                \ committed dispatches to its own arm
TT-AR-RT-REV 0 T=                                \ and carries its payload through unchanged
TT-AR-RT-CON 2 T=                                \ conflict dispatches to its own arm
TT-AR-RT-DUP 3 T=                                \ duplicate-write dispatches to its own arm
TT-AR-RT-OMI 4 T=                                \ omitted-read dispatches to its own arm
TT-AR-RT-UNA 5 T=                                \ unauthorized dispatches to its own arm
TT-AR-RT-EXH 6 T=                                \ exhausted dispatches to its own arm
TT-AR-RT-DIM 4 T=                                \ and carries `retries`, not a zeroed ordinal
TT-AR-CON-DIM -1 T=                              \ the no-payload arms of TT-AR-DIMN are live
TT-AR-CON-REV 0 T=                               \ the no-payload arms of TT-AR-REV are live
TT-AR-EXH-REV 0 T=                               \ exhausted is not a revision-carrying arm

;package

CSTORE:RESET

T-REPORT
