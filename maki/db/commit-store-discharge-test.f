\ maki/db/commit-store-discharge-test.f - acceptance for the folded obligation-discharge
\ AUTHORITY leg threaded into the commit path (CSTORE:COMMIT-DISCHARGED, maki/db/commit-store.f,
\ dot habu-v2-deterministic-audit-428d27c2). Proves the § 23.9 "who may discharge which obligation"
\ THIRD validate leg composes with the landed capability + budget legs, each property by a named test:
\
\   CD-OK / -HEAD     : discharges + authorized verifier + caps + budget -> committed, HEAD advances
\   CD-AUDIT / -KIND  : a successful discharge records ONE evidence-decision audit event (outcome 0)
\   CD-NOTDISCH/-HEAD : a non-discharging evidence -> not-discharged, HEAD unchanged, NO audit event
\   CD-UNAUTHV/-HEAD  : discharges but the verifier is not on the allowlist -> unauthorized-verifier
\   CD-DISCHARGE-1ST  : the discharge leg fires BEFORE the capability leg (a non-discharge under a
\                       weak grant is not-discharged, not unauthorized)
\   CD-CAP-UNAUTH     : discharges, but the grant lacks the txn caps -> unauthorized (capability leg)
\   CD-EXHAUST/-DIM   : discharges + authorized, but the ledger is too small -> exhausted
\   CD-CHARGE / -NOCH : a committed discharge charges once; a refused discharge charges nothing

require lib/prelude.f
require lib/test.f
require test/checker-assert.f
require lib/fs.f
require maki/db/commit-store.f
require maki/db/transaction.f
require maki/db/capability.f
require maki/db/budget-ledger.f
require maki/db/budget-dim.f
require maki/db/obligation.f
require maki/db/promotion-authority.f
require maki/db/audit-log.f
require maki/artifact.f
require maki/config.f
require maki/producer.f
require maki/rev.f

package CSTORE-DISCHARGE-TEST

: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO  ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;

s" hb-cd-test" TMPDIR-MKDIR CSTORE:ROOT!

\ ---- store fixtures -------------------------------------------------------------
: OBJ-A ( -- CAD-KIND:artifact-id )   s" cd-obj-a" ARTIFACT:REGISTER ;
: OBJ-C ( -- CAD-KIND:artifact-id )   s" cd-obj-c" ARTIFACT:REGISTER ;
: G0 ( -- CAD-KIND:rev-id )           s" cd-genesis" REV:COMMIT ;
: GENESIS ( -- )   CSTORE:RESET  G0 CSTORE:INIT-HEAD  AUDIT:RESET ;

\ A txn declaring capability codes {1,2} (mask 3) and a compute-time reserve of 40.
: MK-CAP-TXN ( -- txn )
   G0 TX:OPEN
   OBJ-A TX:PRESENT TX:READ+  OBJ-C TX:WRITE+  OBJ-A TX:DEP+
   1 TX:CAP+  2 TX:CAP+
   BUDGET-DIM:COMPUTE-TIME BUDGET:DIM>N 40 TX:BUDGET+
   TX:BUILD ;

: MK-GRANT-FULL ( -- CAPTOK:grant )
   CAPTOK:RESET CAPTOK:NEW  7 CAPTOK:CAP!  BUDGET-DIM:COMPUTE-TIME 100 CAPTOK:BUDGET!  CAPTOK:ROOT ;
: MK-GRANT-WEAK ( -- CAPTOK:grant )
   CAPTOK:RESET CAPTOK:NEW  1 CAPTOK:CAP!  BUDGET-DIM:COMPUTE-TIME 100 CAPTOK:BUDGET!  CAPTOK:ROOT ;
: MK-LEDGER ( -- LEDGER:ledger )
   LEDGER:RESET LEDGER:OPEN {: l:LEDGER:ledger :}  l BUDGET-DIM:COMPUTE-TIME 100 LEDGER:LIMIT!  l ;
: MK-LEDGER-SMALL ( -- LEDGER:ledger )
   LEDGER:RESET LEDGER:OPEN {: l:LEDGER:ledger :}  l BUDGET-DIM:COMPUTE-TIME 20 LEDGER:LIMIT!  l ;

\ ---- discharge fixtures (the promotion-authority-test canonical obligation/evidence) --
: SUBJ ( -- CAD-KIND:artifact-id )    s" cd/subj" ARTIFACT:REGISTER ;
: OTHER-SUBJ ( -- CAD-KIND:artifact-id ) s" cd/subj-2" ARTIFACT:REGISTER ;
: ENV ( -- CAD-KIND:config-id )       s" cd/env" CONFIG:REGISTER ;
: PROD ( -- CAD-KIND:producer-id )    s" cd/agent-search" PRODUCER:REGISTER ;
: VERIF ( -- CAD-KIND:producer-id )   s" cd/verifier-diff" PRODUCER:REGISTER ;
: VERIF2 ( -- CAD-KIND:producer-id )  s" cd/verifier-other" PRODUCER:REGISTER ;

: OBL-CANON ( -- OBLIG:obligation )
   OBLIG:NEW
   SUBJ OBLIG:SUBJECT
   OBLIG-RELATION:SEMANTIC-EQUIV OBLIG:RELATION
   OBLIG-DOMAIN:EXACT OBLIG:DOMAIN
   OBLIG-INDEPENDENCE:INDEPENDENT OBLIG:POLICY
   OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:VERIFIER-CLASS
   ENV OBLIG:ENVIRONMENT
   PROD OBLIG:PRODUCER
   OBLIG:SEAL ;
: EV-MATCH ( -- OBLIG:evidence )       \ verifier VERIF (authorized), discharges
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;
: EV-VERIF2 ( -- OBLIG:evidence )      \ verifier VERIF2 (NOT authorized), discharges
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF2 OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;
: EV-WRONGSUBJ ( -- OBLIG:evidence )   \ wrong subject -> does not discharge
   OTHER-SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;
: AUTH ( -- DAUTH:authority )          \ authorizes VERIF only
   DAUTH:NEW  VERIF DAUTH:AUTHORIZE+  DAUTH:SEAL ;

\ ---- commit-discharge-result decoders ------------------------------------------
: DCODE ( commit-discharge-result<CAD-KIND:rev-id> -- n )
   MATCH CSTORE:commit-discharge-result
      committed             OF drop 0 ENDOF
      conflict              OF 1 ENDOF
      duplicate-write       OF 2 ENDOF
      omitted-read          OF 3 ENDOF
      unauthorized          OF 4 ENDOF
      exhausted             OF drop 5 ENDOF
      not-discharged        OF 6 ENDOF
      unauthorized-verifier OF 7 ENDOF
   ;MATCH ;
: DEXDIM ( commit-discharge-result<CAD-KIND:rev-id> -- n )   \ exhausted dimension ordinal, else -1
   MATCH CSTORE:commit-discharge-result
      committed             OF drop -1 ENDOF
      conflict              OF -1 ENDOF
      duplicate-write       OF -1 ENDOF
      omitted-read          OF -1 ENDOF
      unauthorized          OF -1 ENDOF
      exhausted             OF BUDGET:DIM>N ENDOF
      not-discharged        OF -1 ENDOF
      unauthorized-verifier OF -1 ENDOF
   ;MATCH ;
: EKN ( AUDIT:event-kind -- n )
   MATCH AUDIT:event-kind
      action-request    OF 0 ENDOF
      action-result     OF 1 ENDOF
      txn-commit        OF 2 ENDOF
      verifier-run      OF 3 ENDOF
      evidence-decision OF 4 ENDOF
      promotion         OF 5 ENDOF
      activation        OF 6 ENDOF
      rollback          OF 7 ENDOF
   ;MATCH ;

\ ================================================================================
\ authorized discharge -> committed
\ ================================================================================
: CD-OK ( -- n )
   GENESIS  MK-CAP-TXN MK-GRANT-FULL MK-LEDGER OBL-CANON EV-MATCH AUTH
   CSTORE:COMMIT-DISCHARGED DCODE ;
: CD-OK-HEAD ( -- bool )
   GENESIS  MK-CAP-TXN {: t:txn :}  t TX:PROPOSE {: r:CAD-KIND:rev-id :}
   t MK-GRANT-FULL MK-LEDGER OBL-CANON EV-MATCH AUTH CSTORE:COMMIT-DISCHARGED drop
   r CSTORE:HEAD-IS? ;
: CD-AUDIT ( -- n )                              \ a successful discharge records ONE audit event
   GENESIS  MK-CAP-TXN MK-GRANT-FULL MK-LEDGER OBL-CANON EV-MATCH AUTH CSTORE:COMMIT-DISCHARGED drop
   AUDIT:COUNT ;
: CD-AUDIT-KIND ( -- n )                         \ that event is an evidence-decision (4)
   GENESIS  MK-CAP-TXN MK-GRANT-FULL MK-LEDGER OBL-CANON EV-MATCH AUTH CSTORE:COMMIT-DISCHARGED drop
   0 AUDIT:EVENT-KIND@ EKN ;
: CD-AUDIT-AUX ( -- n )                          \ outcome ordinal 0 (authorized discharge)
   GENESIS  MK-CAP-TXN MK-GRANT-FULL MK-LEDGER OBL-CANON EV-MATCH AUTH CSTORE:COMMIT-DISCHARGED drop
   0 AUDIT:EVENT-AUX@ ;

\ ================================================================================
\ the discharge leg rejects (before any publish / charge)
\ ================================================================================
: CD-NOTDISCH ( -- n )
   GENESIS  MK-CAP-TXN MK-GRANT-FULL MK-LEDGER OBL-CANON EV-WRONGSUBJ AUTH
   CSTORE:COMMIT-DISCHARGED DCODE ;
: CD-NOTDISCH-HEAD ( -- bool )                   \ no publish: HEAD still genesis
   GENESIS  MK-CAP-TXN MK-GRANT-FULL MK-LEDGER OBL-CANON EV-WRONGSUBJ AUTH CSTORE:COMMIT-DISCHARGED drop
   G0 CSTORE:HEAD-IS? ;
: CD-NOTDISCH-NOAUDIT ( -- n )                   \ a refused discharge records NO decision event
   GENESIS  MK-CAP-TXN MK-GRANT-FULL MK-LEDGER OBL-CANON EV-WRONGSUBJ AUTH CSTORE:COMMIT-DISCHARGED drop
   AUDIT:COUNT ;
: CD-UNAUTHV ( -- n )                            \ discharges but verifier not on the allowlist
   GENESIS  MK-CAP-TXN MK-GRANT-FULL MK-LEDGER OBL-CANON EV-VERIF2 AUTH
   CSTORE:COMMIT-DISCHARGED DCODE ;
: CD-UNAUTHV-HEAD ( -- bool )
   GENESIS  MK-CAP-TXN MK-GRANT-FULL MK-LEDGER OBL-CANON EV-VERIF2 AUTH CSTORE:COMMIT-DISCHARGED drop
   G0 CSTORE:HEAD-IS? ;
: CD-DISCHARGE-1ST ( -- n )                      \ discharge leg fires BEFORE the capability leg
   GENESIS  MK-CAP-TXN MK-GRANT-WEAK MK-LEDGER OBL-CANON EV-WRONGSUBJ AUTH
   CSTORE:COMMIT-DISCHARGED DCODE ;             \ weak grant would be unauthorized, but not-discharged wins

\ ================================================================================
\ the capability + budget legs still gate after a successful discharge
\ ================================================================================
: CD-CAP-UNAUTH ( -- n )
   GENESIS  MK-CAP-TXN MK-GRANT-WEAK MK-LEDGER OBL-CANON EV-MATCH AUTH
   CSTORE:COMMIT-DISCHARGED DCODE ;
: CD-EXHAUST ( -- n )
   GENESIS  MK-CAP-TXN MK-GRANT-FULL MK-LEDGER-SMALL OBL-CANON EV-MATCH AUTH
   CSTORE:COMMIT-DISCHARGED DCODE ;
: CD-EXHAUST-DIM ( -- n )
   GENESIS  MK-CAP-TXN MK-GRANT-FULL MK-LEDGER-SMALL OBL-CANON EV-MATCH AUTH
   CSTORE:COMMIT-DISCHARGED DEXDIM ;

\ ================================================================================
\ charge exactly on a committed discharge; nothing on a refusal
\ ================================================================================
: CD-CHARGE ( -- n )
   GENESIS  MK-GRANT-FULL {: g:CAPTOK:grant :}  MK-LEDGER {: l:LEDGER:ledger :}
   MK-CAP-TXN g l OBL-CANON EV-MATCH AUTH CSTORE:COMMIT-DISCHARGED drop
   l BUDGET-DIM:COMPUTE-TIME LEDGER:REMAINING@ ;
: CD-NOCH ( -- n )                               \ a not-discharged commit charges nothing
   GENESIS  MK-GRANT-FULL {: g:CAPTOK:grant :}  MK-LEDGER {: l:LEDGER:ledger :}
   MK-CAP-TXN g l OBL-CANON EV-WRONGSUBJ AUTH CSTORE:COMMIT-DISCHARGED drop
   l BUDGET-DIM:COMPUTE-TIME LEDGER:REMAINING@ ;

T-RESET

\ authorized discharge -> committed + audit
CD-OK 0 T=
CD-OK-HEAD TTRUE
CD-AUDIT 1 T=
CD-AUDIT-KIND 4 T=
CD-AUDIT-AUX 0 T=

\ discharge-leg rejects (no publish / no audit / no charge)
CD-NOTDISCH 6 T=
CD-NOTDISCH-HEAD TTRUE
CD-NOTDISCH-NOAUDIT 0 T=
CD-UNAUTHV 7 T=
CD-UNAUTHV-HEAD TTRUE
CD-DISCHARGE-1ST 6 T=

\ capability + budget legs still gate
CD-CAP-UNAUTH 4 T=
CD-EXHAUST 5 T=
CD-EXHAUST-DIM 0 T=

\ charge accounting
CD-CHARGE 60 T=
CD-NOCH 100 T=

\ ---- the generated commit-discharge-result constructors: spelling + effect ------
\ commit-discharge-result is declared through the unified ENUM front end in full mode, so
\ these pins are the migration's identity proof and must keep holding for every later
\ declaration change. The SPELLING is load-bearing: the checker answers 1 (uncheckable)
\ for a name it cannot resolve, and YES demands -1, so a -1 means the checker resolved
\ EXACTLY this constructor name; NO demands 0, which it can only reach after resolving the
\ name and refusing the types. This family's constructor package
\ CSTORE-COMMIT--DISCHARGE--RESULT is exactly 32 bytes, which is TF-CTOR-NAME-LIMIT
\ (src/core/type-family.f), so these eight pins are also the tripwire for a rename that
\ would push the whole family onto the opaque hashed spelling.
s" CDR-C-COM ( CAD-KIND:rev-id -- CSTORE:commit-discharge-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--DISCHARGE--RESULT:COMMITTED" YES
s" CDR-C-CON ( -- CSTORE:commit-discharge-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--DISCHARGE--RESULT:CONFLICT" YES
s" CDR-C-DUP ( -- CSTORE:commit-discharge-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--DISCHARGE--RESULT:DUPLICATE-WRITE" YES
s" CDR-C-OMI ( -- CSTORE:commit-discharge-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--DISCHARGE--RESULT:OMITTED-READ" YES
s" CDR-C-UNA ( -- CSTORE:commit-discharge-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--DISCHARGE--RESULT:UNAUTHORIZED" YES
s" CDR-C-EXH ( BUDGET:dim -- CSTORE:commit-discharge-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--DISCHARGE--RESULT:EXHAUSTED" YES
s" CDR-C-ND ( -- CSTORE:commit-discharge-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--DISCHARGE--RESULT:NOT-DISCHARGED" YES
s" CDR-C-UV ( -- CSTORE:commit-discharge-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--DISCHARGE--RESULT:UNAUTHORIZED-VERIFIER" YES
\ Forge negatives: a raw cell cannot fill the committed payload, the result is not a bare
\ scalar, the payload is mandatory, a same-width FOREIGN identity role cannot stand in for
\ the revision, and neither payload role can stand in for the other.
s" CDR-F-RAW ( n -- CSTORE:commit-discharge-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--DISCHARGE--RESULT:COMMITTED" NO
s" CDR-F-BARE ( CAD-KIND:rev-id -- n ) CSTORE-COMMIT--DISCHARGE--RESULT:COMMITTED" NO
s" CDR-F-NONE ( -- CSTORE:commit-discharge-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--DISCHARGE--RESULT:COMMITTED" NO
s" CDR-F-FGN ( CAD-KIND:artifact-id -- CSTORE:commit-discharge-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--DISCHARGE--RESULT:COMMITTED" NO
s" CDR-F-EXH-AS-REV ( CAD-KIND:rev-id -- CSTORE:commit-discharge-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--DISCHARGE--RESULT:EXHAUSTED" NO
s" CDR-F-COM-AS-DIM ( BUDGET:dim -- CSTORE:commit-discharge-result<CAD-KIND:rev-id> ) CSTORE-COMMIT--DISCHARGE--RESULT:COMMITTED" NO

\ ---- swapped-payload MATCH negatives (the eight-arm family's own risk) ----------
\ Exhaustiveness already forces every arm to appear, so the residual naming risk in an
\ eight-arm family with two DIFFERENT payload roles is an arm reading the wrong role. The
\ first candidate is the positive control: the real arm order with each payload bound to a
\ typed local of its declared role. The two swapped candidates keep that exact shape and
\ only exchange which role each payload arm binds, so a 0 from them is the checker
\ refusing the swap, not refusing the shape.
s" CDR-M-OK ( CSTORE:commit-discharge-result<CAD-KIND:rev-id> -- n ) MATCH CSTORE:commit-discharge-result committed OF {: r:CAD-KIND:rev-id :} 1 ENDOF conflict OF 2 ENDOF duplicate-write OF 3 ENDOF omitted-read OF 4 ENDOF unauthorized OF 5 ENDOF exhausted OF {: d:BUDGET:dim :} 6 ENDOF not-discharged OF 7 ENDOF unauthorized-verifier OF 8 ENDOF ;MATCH" YES
s" CDR-M-SWAP-A ( CSTORE:commit-discharge-result<CAD-KIND:rev-id> -- n ) MATCH CSTORE:commit-discharge-result committed OF {: d:BUDGET:dim :} 1 ENDOF conflict OF 2 ENDOF duplicate-write OF 3 ENDOF omitted-read OF 4 ENDOF unauthorized OF 5 ENDOF exhausted OF {: d:BUDGET:dim :} 6 ENDOF not-discharged OF 7 ENDOF unauthorized-verifier OF 8 ENDOF ;MATCH" NO
s" CDR-M-SWAP-B ( CSTORE:commit-discharge-result<CAD-KIND:rev-id> -- n ) MATCH CSTORE:commit-discharge-result committed OF {: r:CAD-KIND:rev-id :} 1 ENDOF conflict OF 2 ENDOF duplicate-write OF 3 ENDOF omitted-read OF 4 ENDOF unauthorized OF 5 ENDOF exhausted OF {: r:CAD-KIND:rev-id :} 6 ENDOF not-discharged OF 7 ENDOF unauthorized-verifier OF 8 ENDOF ;MATCH" NO

public

\ twin is CSTORE:commit-discharge-result's SHAPE under a different name: same arity, same
\ eight variants in the same order, same two named payload fields. It exists only so the
\ negatives below can prove commit-outcome identity is NOMINAL - two identically shaped
\ ENUM families never unify, in either direction. It has to be public: a private family
\ publishes no constructors at all, and the positive control below builds through the
\ twin's own committed, so neither negative can pass by being unresolvable rather than
\ ill-typed. The tail is deliberately SHORT: with tail `cdr-twin` the generated package
\ would be 33 bytes, one over TF-CTOR-NAME-LIMIT, and every twin constructor would
\ silently take the opaque hashed spelling instead.
ENUM twin 1
   VARIANT committed FIELD rev a ;VARIANT
   VARIANT conflict ;VARIANT
   VARIANT duplicate-write ;VARIANT
   VARIANT omitted-read ;VARIANT
   VARIANT unauthorized ;VARIANT
   VARIANT exhausted FIELD dim BUDGET:dim ;VARIANT
   VARIANT not-discharged ;VARIANT
   VARIANT unauthorized-verifier ;VARIANT
;ENUM

private

s" CDR-C-TWIN ( CAD-KIND:rev-id -- twin<CAD-KIND:rev-id> ) CSTORE--DISCHARGE--TEST-TWIN:COMMITTED" YES
s" CDR-C-TWIN-X1 ( CAD-KIND:rev-id -- twin<CAD-KIND:rev-id> ) CSTORE-COMMIT--DISCHARGE--RESULT:COMMITTED" NO
s" CDR-C-TWIN-X2 ( CAD-KIND:rev-id -- CSTORE:commit-discharge-result<CAD-KIND:rev-id> ) CSTORE--DISCHARGE--TEST-TWIN:COMMITTED" NO

;package

\ ---- every variant constructs and dispatches through MATCH ---------------------
\ The tests above reach the commit-discharge-result arms only through
\ CSTORE:COMMIT-DISCHARGED. The variant producers CD-COMMITTED .. CD-UNAUTH-VERIFIER are
\ owner-private, so reopen the owning package to construct each of the eight variants
\ DIRECTLY and match it straight back; that is what proves both named payload FIELDs bind
\ in declaration order. Each payload arm binds to a TYPED local: the committed round-trip
\ demands the recovered revision equal the one constructed AND differ from a second live
\ revision, and the exhausted round-trip uses `retries` (ordinal 4) rather than
\ `compute-time`, because compute-time's ordinal is 0 and a dropped or zeroed dimension
\ payload would read back as 0 and pass. The CD-EXHAUST-DIM leg above is exactly that
\ zero-ordinal case.
\
\ Construction is factored into one typed word per variant because the checker requires
\ MATCH's scrutinee to be a concretely instantiated family value: a single word that both
\ constructs and matches is refused, and the diagnostic names the family token as an
\ undefined word. That refusal predates this migration (it reproduces identically on the
\ legacy declaration) and is reported separately. Names carry a per-family prefix because
\ all three commit-store suites reopen this one package in the same process.
package CSTORE

: TT-CDR-REV-A ( -- CAD-KIND:rev-id )   s" c4-cdr-rt-a" REV:COMMIT ;
: TT-CDR-REV-B ( -- CAD-KIND:rev-id )   s" c4-cdr-rt-b" REV:COMMIT ;
: TT-CDR-DIM ( -- BUDGET:dim )          BUDGET-DIM:RETRIES ;   \ ordinal 4, deliberately non-zero

: TT-CDR-MK-COM ( CAD-KIND:rev-id -- commit-discharge-result<CAD-KIND:rev-id> ) CD-COMMITTED ;
: TT-CDR-MK-CON ( -- commit-discharge-result<CAD-KIND:rev-id> )   CD-CONFLICT ;
: TT-CDR-MK-DUP ( -- commit-discharge-result<CAD-KIND:rev-id> )   CD-DUP-WRITE ;
: TT-CDR-MK-OMI ( -- commit-discharge-result<CAD-KIND:rev-id> )   CD-OMITTED ;
: TT-CDR-MK-UNA ( -- commit-discharge-result<CAD-KIND:rev-id> )   CD-UNAUTHORIZED ;
: TT-CDR-MK-EXH ( BUDGET:dim -- commit-discharge-result<CAD-KIND:rev-id> ) CD-EXHAUSTED ;
: TT-CDR-MK-ND ( -- commit-discharge-result<CAD-KIND:rev-id> )    CD-NOT-DISCHARGED ;
: TT-CDR-MK-UV ( -- commit-discharge-result<CAD-KIND:rev-id> )    CD-UNAUTH-VERIFIER ;

: TT-CDR-ARM ( commit-discharge-result<CAD-KIND:rev-id> -- n )   \ 1 committed .. 8 unauth-verifier
   MATCH commit-discharge-result
      committed             OF drop 1 ENDOF
      conflict              OF 2 ENDOF
      duplicate-write       OF 3 ENDOF
      omitted-read          OF 4 ENDOF
      unauthorized          OF 5 ENDOF
      exhausted             OF drop 6 ENDOF
      not-discharged        OF 7 ENDOF
      unauthorized-verifier OF 8 ENDOF
   ;MATCH ;

: TT-CDR-REV ( commit-discharge-result<CAD-KIND:rev-id> -- CAD-KIND:rev-id bool )
   MATCH commit-discharge-result
      committed             OF true ENDOF
      conflict              OF TT-CDR-REV-B false ENDOF
      duplicate-write       OF TT-CDR-REV-B false ENDOF
      omitted-read          OF TT-CDR-REV-B false ENDOF
      unauthorized          OF TT-CDR-REV-B false ENDOF
      exhausted             OF drop TT-CDR-REV-B false ENDOF
      not-discharged        OF TT-CDR-REV-B false ENDOF
      unauthorized-verifier OF TT-CDR-REV-B false ENDOF
   ;MATCH ;

: TT-CDR-DIMN ( commit-discharge-result<CAD-KIND:rev-id> -- n )   \ exhausted ordinal, else -1
   MATCH commit-discharge-result
      committed             OF drop -1 ENDOF
      conflict              OF -1 ENDOF
      duplicate-write       OF -1 ENDOF
      omitted-read          OF -1 ENDOF
      unauthorized          OF -1 ENDOF
      exhausted             OF {: d:BUDGET:dim :} d BUDGET:DIM>N ENDOF
      not-discharged        OF -1 ENDOF
      unauthorized-verifier OF -1 ENDOF
   ;MATCH ;

: TT-CDR-AB-DIFF ( -- bool )      TT-CDR-REV-A TT-CDR-REV-B REV:EQUAL? 0= ;
: TT-CDR-DIM-NONZERO ( -- bool )  TT-CDR-DIM BUDGET:DIM>N 0<> ;

: TT-CDR-RT-ARM ( -- n )   TT-CDR-REV-A TT-CDR-MK-COM TT-CDR-ARM ;
: TT-CDR-RT-REV ( -- n )                         \ 0 = payload is A, and is NOT B
   TT-CDR-REV-A TT-CDR-MK-COM TT-CDR-REV {: got:CAD-KIND:rev-id found:bool :}
   found 0= if 1 exit then
   got TT-CDR-REV-A REV:EQUAL? 0= if 2 exit then
   got TT-CDR-REV-B REV:EQUAL? if 3 exit then
   0 ;
: TT-CDR-RT-CON ( -- n )   TT-CDR-MK-CON TT-CDR-ARM ;
: TT-CDR-RT-DUP ( -- n )   TT-CDR-MK-DUP TT-CDR-ARM ;
: TT-CDR-RT-OMI ( -- n )   TT-CDR-MK-OMI TT-CDR-ARM ;
: TT-CDR-RT-UNA ( -- n )   TT-CDR-MK-UNA TT-CDR-ARM ;
: TT-CDR-RT-EXH ( -- n )   TT-CDR-DIM TT-CDR-MK-EXH TT-CDR-ARM ;
: TT-CDR-RT-ND ( -- n )    TT-CDR-MK-ND TT-CDR-ARM ;
: TT-CDR-RT-UV ( -- n )    TT-CDR-MK-UV TT-CDR-ARM ;
: TT-CDR-RT-DIM ( -- n )   TT-CDR-DIM TT-CDR-MK-EXH TT-CDR-DIMN ;
: TT-CDR-UV-DIM ( -- n )   TT-CDR-MK-UV TT-CDR-DIMN ;           \ payloadless: no dimension
: TT-CDR-ND-REV ( -- n )                         \ payloadless: no revision
   TT-CDR-MK-ND TT-CDR-REV {: got:CAD-KIND:rev-id found:bool :}
   found if 1 else 0 then ;

TT-CDR-AB-DIFF TTRUE                             \ the distinguishability control is real
TT-CDR-DIM-NONZERO TTRUE                         \ and the dimension under test is not ordinal 0
TT-CDR-RT-ARM 1 T=                               \ committed dispatches to its own arm
TT-CDR-RT-REV 0 T=                               \ and carries its payload through unchanged
TT-CDR-RT-CON 2 T=                               \ conflict dispatches to its own arm
TT-CDR-RT-DUP 3 T=                               \ duplicate-write dispatches to its own arm
TT-CDR-RT-OMI 4 T=                               \ omitted-read dispatches to its own arm
TT-CDR-RT-UNA 5 T=                               \ unauthorized dispatches to its own arm
TT-CDR-RT-EXH 6 T=                               \ exhausted dispatches to its own arm
TT-CDR-RT-ND 7 T=                                \ not-discharged dispatches to its own arm
TT-CDR-RT-UV 8 T=                                \ unauthorized-verifier dispatches to its own arm
TT-CDR-RT-DIM 4 T=                               \ exhausted carries `retries`, not a zeroed ordinal
TT-CDR-UV-DIM -1 T=                              \ the no-payload arms of TT-CDR-DIMN are live
TT-CDR-ND-REV 0 T=                               \ the no-payload arms of TT-CDR-REV are live

;package

CSTORE:RESET

T-REPORT
