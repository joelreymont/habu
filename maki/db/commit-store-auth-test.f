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
require lib/fs.f
require maki/db/commit-store.f
require maki/db/transaction.f
require maki/db/capability.f
require maki/db/budget-ledger.f
require maki/db/budget-dim.f
require maki/artifact.f
require maki/rev.f

package CSTORE-AUTH-TEST

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

CSTORE:RESET

T-REPORT

;package
