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

CSTORE:RESET

T-REPORT

;package
