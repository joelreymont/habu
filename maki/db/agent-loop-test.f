\ maki/db/agent-loop-test.f - checked acceptance for the bounded autonomous agent-loop
\ controller (maki/db/agent-loop.f, dot habu-v2-bounded-autonomous-1c598fcf). Proves the
\ dot acceptance against a REAL private commit store + the landed grant/ledger/registry/
\ applicability substrate, each property by a named test:
\
\   the loop core (§ 23.2):
\     AL-PROMOTE / AL-PROMOTE-N : a good chooser + rising metric PROMOTES, committing once
\     AL-APPLIC-PROMOTE / -M    : the FOCUSED VERIFIER path - the metric is APPLIC:VERDICT over
\                                 tracked obligations; a chooser that stages discharging evidence
\                                 drives the obligation delta to the goal and promotes
\   (b) a raw command / edit cannot bypass the registry:
\     AL-ORD-* (static)         : the untrusted chooser is a typed `[ -- n txn ]` quotation - a
\                                 missing txn or a raw-n in the txn slot is a checker REJECT (verdict 0)
\     AL-ILLEGAL-ORD / -HEAD    : an out-of-range ordinal -> typed blocked(illegal-action), no mutation
\     AL-ILLEGAL-DECL           : a declared/wrong-kind action -> blocked(illegal-action) (DISPATCH gate)
\   (c) injected non-progress TERMINATES (bounded, never infinite):
\     AL-NONPROG / -N           : a non-progressing chooser hits the non-progress bound -> blocked
\     AL-ITER / -N              : a progressing-but-never-reaching chooser hits the iteration bound
\   (e) authority + budgets hold:
\     AL-EFF-UNAUTH             : an ungranted DISPATCH effect -> blocked(unauthorized)
\     AL-CAP-UNAUTH / -HEAD     : a child grant lacking the txn's caps -> blocked(unauthorized), no publish
\     AL-EXHAUST / -HEAD        : an exhausted ledger -> blocked(budget-exhausted), no partial state
\     AL-ATTEN-CAP / -HEAD / -C : an ESCAPING attenuation (cap bit the parent lacks) -> unauthorized, no mutation
\     AL-ATTEN-BUD              : an ESCAPING attenuation (ceiling above the parent) -> unauthorized
\   (a) replay WITHOUT the chooser yields identical state:
\     AL-REPLAY                 : the loop's decision journal, re-applied directly (no chooser),
\                                 reproduces the same idempotency key, HEAD, and ledger digest
\   (d) crash/retry is idempotent:
\     AL-IDEMP / -HEAD          : re-running a committed step charges the ledger once, HEAD stable
\   capacity / fail-closed throws:
\     AL-TRK-OVERFLOW / AL-JRN-RANGE

require lib/test.f
require lib/fs.f
require test/checker-assert.f
require maki/db/agent-loop.f
require maki/db/action.f
require maki/db/capability.f
require maki/db/budget-ledger.f
require maki/db/budget-dim.f
require maki/db/commit-store.f
require maki/db/transaction.f
require maki/db/obligation.f
require maki/db/evidence-applicability.f
require maki/artifact.f
require maki/config.f
require maki/producer.f
require maki/rev.f

package ALOOP-TEST

\ One private store dir for the whole suite; each loop test RESETs the store first.
s" hb-aloop-test" TMPDIR-MKDIR CSTORE:ROOT!

32 constant TRK-CAP                 \ mirrors ALOOP TRK-CAP (private) for the overflow probe
create JKBUF 32 allot               \ JOURNAL-KEY@ output scratch
create K1 32 allot   create K2 32 allot
create DG1 32 allot   create DG2 32 allot

\ ---- store fixtures -------------------------------------------------------------
: OBJ-A ( -- CAD-KIND:artifact-id )   s" aloop-obj-a" ARTIFACT:REGISTER ;
: OBJ-C ( -- CAD-KIND:artifact-id )   s" aloop-obj-c" ARTIFACT:REGISTER ;
: G0 ( -- CAD-KIND:rev-id )           s" aloop-genesis" REV:COMMIT ;
: GENESIS ( -- )   CSTORE:RESET  G0 CSTORE:INIT-HEAD ;

\ A valid commit txn declaring capability codes {1,2} (mask 3) + a compute-time reserve of 40.
: MK-COMMIT-TXN ( -- txn )
   G0 TX:OPEN
   OBJ-A TX:PRESENT TX:READ+  OBJ-C TX:WRITE+  OBJ-A TX:DEP+
   1 TX:CAP+  2 TX:CAP+
   BUDGET-DIM:COMPUTE-TIME BUDGET:DIM>N 40 TX:BUDGET+
   TX:BUILD ;

\ ---- authority / budget fixtures ------------------------------------------------
: MK-ROOT ( -- CAPTOK:grant )      \ caps {0,1,2} (mask 7) + a 100 compute-time ceiling
   CAPTOK:RESET CAPTOK:NEW  7 CAPTOK:CAP!  BUDGET-DIM:COMPUTE-TIME 100 CAPTOK:BUDGET!  CAPTOK:ROOT ;
: MK-LEDGER ( -- LEDGER:ledger )
   LEDGER:RESET LEDGER:OPEN {: l:LEDGER:ledger :}  l BUDGET-DIM:COMPUTE-TIME 100 LEDGER:LIMIT!  l ;
: MK-LEDGER-SMALL ( -- LEDGER:ledger )
   LEDGER:RESET LEDGER:OPEN {: l:LEDGER:ledger :}  l BUDGET-DIM:COMPUTE-TIME 20 LEDGER:LIMIT!  l ;

\ ---- controller configuration ---------------------------------------------------
: CONFIG-STD ( n -- ) {: goal:n :}   \ full effect grant, child caps 7 + 100 compute ceiling, bounds
   ALOOP:RESET
   63 ALOOP:EFFECT-GRANT!
   7 ALOOP:CHILD-CAP!
   BUDGET-DIM:COMPUTE-TIME 100 ALOOP:CHILD-BUDGET!
   goal ALOOP:GOAL!
   10 ALOOP:MAX-ITERS!
   5 ALOOP:MAX-NONPROGRESS! ;

\ ---- scripted metric world (the trusted verifier for the bounds/authority tests) -----
variable METRIC-V
: METRIC ( -- n )   METRIC-V @ ;

\ ---- enumeration-ordinal resolution --------------------------------------------
: ORD-OF ( ptr u8 n -- n )                       \ the ACTION enumeration index of a registered name
   ACTION:ID-OF {: id:CAD-KIND:action-id :}
   0 begin dup ACTION:COUNT < while
      dup ACTION:ENUM-AT id ACTION:EQUAL? if exit then
      1+
   repeat drop -1 ;
: COMMIT-ORD ( -- n )   s" TX:COMMIT" ORD-OF ;

\ ---- scripted choosers (deterministic; each returns the untrusted proposal (ordinal, txn)) ----
\ GOOD advances the scripted metric world AND proposes the legal commit action + a valid txn.
: GOOD-CHOOSE ( -- n txn )   1 METRIC-V +!  COMMIT-ORD MK-COMMIT-TXN ;
\ STALL proposes the legal commit but never advances the metric (idempotent after the first publish).
: STALL-CHOOSE ( -- n txn )  COMMIT-ORD MK-COMMIT-TXN ;
\ ADVERSARIAL: an out-of-range action ordinal (an unregistered action).
: EVIL-ORD-CHOOSE ( -- n txn )   999 MK-COMMIT-TXN ;
\ ADVERSARIAL: a `declared`/wrong-kind action (PASS:RUN wants pass-obj, is not implemented).
: EVIL-DECL-CHOOSE ( -- n txn )  s" PASS:RUN" ORD-OF MK-COMMIT-TXN ;

\ ---- obligation / evidence fixtures for the APPLIC focused-verifier path --------
: A-SUBJ ( -- CAD-KIND:artifact-id )   s" aloop-test/subj" ARTIFACT:REGISTER ;
: A-ENV ( -- CAD-KIND:config-id )      s" aloop-test/env" CONFIG:REGISTER ;
: A-PROD ( -- CAD-KIND:producer-id )   s" aloop-test/prod" PRODUCER:REGISTER ;
: A-VERIF ( -- CAD-KIND:producer-id )  s" aloop-test/verif" PRODUCER:REGISTER ;
: A-OBL ( -- OBLIG:obligation )
   OBLIG:NEW
   A-SUBJ OBLIG:SUBJECT
   OBLIG-RELATION:SEMANTIC-EQUIV OBLIG:RELATION
   OBLIG-DOMAIN:EXACT OBLIG:DOMAIN
   OBLIG-INDEPENDENCE:INDEPENDENT OBLIG:POLICY
   OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:VERIFIER-CLASS
   A-ENV OBLIG:ENVIRONMENT
   A-PROD OBLIG:PRODUCER
   OBLIG:SEAL ;
: A-EV ( -- OBLIG:evidence )                     \ discharges A-OBL (subject/domain/relation/env/vc match, independent)
   A-SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV A-ENV A-VERIF OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;
\ The good APPLIC chooser stages the discharging evidence (the world effect of a verified commit),
\ then proposes the legal commit; the controller's own APPLIC:VERDICT then confirms the discharge.
: APPLIC-GOOD-CHOOSE ( -- n txn )   A-EV APPLIC:EVIDENCE+  COMMIT-ORD MK-COMMIT-TXN ;

\ ---- typed-outcome decoders ----------------------------------------------------
: REASON>N ( ALOOP:blocked-reason -- n )
   MATCH ALOOP:blocked-reason
      nonprogress      OF 0 ENDOF
      iter-bound       OF 1 ENDOF
      budget-exhausted OF 2 ENDOF
      unauthorized     OF 3 ENDOF
      illegal-action   OF 4 ENDOF
   ;MATCH ;
: LRCODE ( loop-result -- n )                    \ -1 promoted / 0..4 blocked-reason ordinal
   MATCH ALOOP:loop-result
      promoted OF -1 ENDOF
      blocked  OF REASON>N ENDOF
   ;MATCH ;

: KEY-EQ? ( ptr u8 ptr u8 -- bool ) {: a:ptr b:ptr :}  \ fixed 32-byte compare
   0 begin dup 32 < while
      dup {: k:n :}
      a k + c@  b k + c@  <> if drop false exit then
      1+
   repeat drop true ;

\ ================================================================================
\ the loop core: promote on progress
\ ================================================================================
: AL-PROMOTE ( -- n )
   GENESIS  0 METRIC-V !  1 CONFIG-STD
   MK-ROOT MK-LEDGER [: GOOD-CHOOSE ;] [: METRIC ;] ALOOP:RUN LRCODE ;
: AL-PROMOTE-N ( -- n )                          \ one committed decision reached the goal
   GENESIS  0 METRIC-V !  1 CONFIG-STD
   MK-ROOT MK-LEDGER [: GOOD-CHOOSE ;] [: METRIC ;] ALOOP:RUN drop
   ALOOP:COMMITS ;

\ the focused-verifier (APPLIC:VERDICT) path
: AL-APPLIC-PROMOTE ( -- n )
   GENESIS  APPLIC:RESET
   1 CONFIG-STD
   A-OBL ALOOP:TRACK-OBLIGATION+
   MK-ROOT MK-LEDGER [: APPLIC-GOOD-CHOOSE ;] [: ALOOP:APPLIC-SATISFIED ;] ALOOP:RUN LRCODE ;
: AL-APPLIC-M ( -- n )                           \ the obligation became applicable (metric 1)
   GENESIS  APPLIC:RESET
   1 CONFIG-STD
   A-OBL ALOOP:TRACK-OBLIGATION+
   MK-ROOT MK-LEDGER [: APPLIC-GOOD-CHOOSE ;] [: ALOOP:APPLIC-SATISFIED ;] ALOOP:RUN drop
   ALOOP:FINAL-METRIC ;

\ ================================================================================
\ (c) injected non-progress terminates (bounded)
\ ================================================================================
: AL-NONPROG ( -- n )
   GENESIS  0 METRIC-V !  100 CONFIG-STD  3 ALOOP:MAX-NONPROGRESS!
   MK-ROOT MK-LEDGER [: STALL-CHOOSE ;] [: METRIC ;] ALOOP:RUN LRCODE ;
: AL-NONPROG-N ( -- n )
   GENESIS  0 METRIC-V !  100 CONFIG-STD  3 ALOOP:MAX-NONPROGRESS!
   MK-ROOT MK-LEDGER [: STALL-CHOOSE ;] [: METRIC ;] ALOOP:RUN drop
   ALOOP:NONPROGRESS ;
: AL-ITER ( -- n )                               \ progresses each step but goal 100 is unreachable in 10 iters
   GENESIS  0 METRIC-V !  100 CONFIG-STD
   MK-ROOT MK-LEDGER [: GOOD-CHOOSE ;] [: METRIC ;] ALOOP:RUN LRCODE ;
: AL-ITER-N ( -- n )
   GENESIS  0 METRIC-V !  100 CONFIG-STD
   MK-ROOT MK-LEDGER [: GOOD-CHOOSE ;] [: METRIC ;] ALOOP:RUN drop
   ALOOP:ITERATIONS ;

\ ================================================================================
\ (b) a raw command / edit cannot bypass the registry
\ ================================================================================
: AL-ILLEGAL-ORD ( -- n )
   GENESIS  0 METRIC-V !  100 CONFIG-STD
   MK-ROOT MK-LEDGER [: EVIL-ORD-CHOOSE ;] [: METRIC ;] ALOOP:RUN LRCODE ;
: AL-ILLEGAL-ORD-HEAD ( -- bool )                \ no mutation: HEAD still genesis
   GENESIS  0 METRIC-V !  100 CONFIG-STD
   MK-ROOT MK-LEDGER [: EVIL-ORD-CHOOSE ;] [: METRIC ;] ALOOP:RUN drop
   G0 CSTORE:HEAD-IS? ;
: AL-ILLEGAL-DECL ( -- n )
   GENESIS  0 METRIC-V !  100 CONFIG-STD
   MK-ROOT MK-LEDGER [: EVIL-DECL-CHOOSE ;] [: METRIC ;] ALOOP:RUN LRCODE ;

\ ================================================================================
\ (e) authority + budgets hold
\ ================================================================================
: AL-EFF-UNAUTH ( -- n )                         \ DISPATCH effect axis: only read-store granted
   GENESIS  0 METRIC-V !  100 CONFIG-STD  1 ALOOP:EFFECT-GRANT!
   MK-ROOT MK-LEDGER [: STALL-CHOOSE ;] [: METRIC ;] ALOOP:RUN LRCODE ;
: AL-CAP-UNAUTH ( -- n )                         \ child grant caps {0}; txn declares {1,2} -> COMMIT rejects
   GENESIS  0 METRIC-V !  100 CONFIG-STD  1 ALOOP:CHILD-CAP!
   MK-ROOT MK-LEDGER [: STALL-CHOOSE ;] [: METRIC ;] ALOOP:RUN LRCODE ;
: AL-CAP-UNAUTH-HEAD ( -- bool )
   GENESIS  0 METRIC-V !  100 CONFIG-STD  1 ALOOP:CHILD-CAP!
   MK-ROOT MK-LEDGER [: STALL-CHOOSE ;] [: METRIC ;] ALOOP:RUN drop
   G0 CSTORE:HEAD-IS? ;
: AL-EXHAUST ( -- n )
   GENESIS  0 METRIC-V !  100 CONFIG-STD
   MK-ROOT MK-LEDGER-SMALL [: STALL-CHOOSE ;] [: METRIC ;] ALOOP:RUN LRCODE ;
: AL-EXHAUST-HEAD ( -- bool )
   GENESIS  0 METRIC-V !  100 CONFIG-STD
   MK-ROOT MK-LEDGER-SMALL [: STALL-CHOOSE ;] [: METRIC ;] ALOOP:RUN drop
   G0 CSTORE:HEAD-IS? ;
: AL-ATTEN-CAP ( -- n )                          \ request cap bit 3 (mask 8) the parent (7) lacks -> escape
   GENESIS  0 METRIC-V !  100 CONFIG-STD  8 ALOOP:CHILD-CAP!
   MK-ROOT MK-LEDGER [: STALL-CHOOSE ;] [: METRIC ;] ALOOP:RUN LRCODE ;
: AL-ATTEN-CAP-HEAD ( -- bool )
   GENESIS  0 METRIC-V !  100 CONFIG-STD  8 ALOOP:CHILD-CAP!
   MK-ROOT MK-LEDGER [: STALL-CHOOSE ;] [: METRIC ;] ALOOP:RUN drop
   G0 CSTORE:HEAD-IS? ;
: AL-ATTEN-CAP-C ( -- n )                        \ nothing committed on an escape
   GENESIS  0 METRIC-V !  100 CONFIG-STD  8 ALOOP:CHILD-CAP!
   MK-ROOT MK-LEDGER [: STALL-CHOOSE ;] [: METRIC ;] ALOOP:RUN drop
   ALOOP:COMMITS ;
: AL-ATTEN-BUD ( -- n )                          \ request a 200 ceiling above the parent's 100 -> escape
   GENESIS  0 METRIC-V !  100 CONFIG-STD  BUDGET-DIM:COMPUTE-TIME 200 ALOOP:CHILD-BUDGET!
   MK-ROOT MK-LEDGER [: STALL-CHOOSE ;] [: METRIC ;] ALOOP:RUN LRCODE ;

\ ================================================================================
\ (d) crash/retry is idempotent (the idempotency-key machinery)
\ ================================================================================
: AL-IDEMP ( -- n )                              \ two identical runs over one ledger charge exactly once
   GENESIS  MK-LEDGER {: l:LEDGER:ledger :}
   0 METRIC-V !  1 CONFIG-STD  MK-ROOT l [: GOOD-CHOOSE ;] [: METRIC ;] ALOOP:RUN drop
   0 METRIC-V !  1 CONFIG-STD  MK-ROOT l [: GOOD-CHOOSE ;] [: METRIC ;] ALOOP:RUN drop
   l BUDGET-DIM:COMPUTE-TIME LEDGER:REMAINING@ ;
: AL-IDEMP-HEAD ( -- bool )                      \ HEAD stable at the committed rev across the restart
   GENESIS  MK-COMMIT-TXN TX:PROPOSE {: r:CAD-KIND:rev-id :}
   MK-LEDGER {: l:LEDGER:ledger :}
   0 METRIC-V !  1 CONFIG-STD  MK-ROOT l [: GOOD-CHOOSE ;] [: METRIC ;] ALOOP:RUN drop
   0 METRIC-V !  1 CONFIG-STD  MK-ROOT l [: GOOD-CHOOSE ;] [: METRIC ;] ALOOP:RUN drop
   r CSTORE:HEAD-IS? ;

\ ================================================================================
\ (a) replay without the chooser yields identical state
\ ================================================================================
\ Run the loop, capture its decision journal + HEAD + ledger digest; then re-apply the recorded
\ decision DIRECTLY (no chooser, no loop) on a fresh store + ledger and prove the state is identical.
: AL-REPLAY ( -- bool )
   GENESIS  0 METRIC-V !
   MK-LEDGER {: l1:LEDGER:ledger :}
   1 CONFIG-STD
   MK-ROOT l1 [: GOOD-CHOOSE ;] [: METRIC ;] ALOOP:RUN drop
   ALOOP:JOURNAL-COUNT 1 = {: jc1:bool :}          \ exactly one committed decision
   0 K1 ALOOP:JOURNAL-KEY@
   MK-COMMIT-TXN TX:PROPOSE {: rL:CAD-KIND:rev-id :}
   rL CSTORE:HEAD-IS? {: head1:bool :}
   l1 DG1 32 LEDGER:DIGEST drop
   \ --- replay the recorded decision without the chooser ---
   GENESIS
   MK-LEDGER {: l2:LEDGER:ledger :}
   MK-COMMIT-TXN MK-ROOT l2 CSTORE:COMMIT-AUTHORIZED drop
   MK-COMMIT-TXN K2 32 TX:IDEM-KEY>WIRE drop
   rL CSTORE:HEAD-IS? {: head2:bool :}
   l2 DG2 32 LEDGER:DIGEST drop
   jc1 head1 and head2 and
   K1 K2 KEY-EQ? and
   DG1 DG2 KEY-EQ? and ;

\ ================================================================================
\ capacity / fail-closed throws
\ ================================================================================
: AL-TRK-OVERFLOW ( -- )                          \ one past TRK-CAP tracked obligations -> E-ALOOP-CAP
   ALOOP:RESET  A-OBL {: o:OBLIG:obligation :}
   TRK-CAP 1+ 0 ?do  o ALOOP:TRACK-OBLIGATION+  loop ;
: AL-JRN-RANGE ( -- )                             \ a journal index with no recorded decision -> E-ALOOP-RANGE
   ALOOP:RESET  0 JKBUF ALOOP:JOURNAL-KEY@ ;

\ ---- static verdict fixtures: the untrusted chooser must produce the typed proposal ----
: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO ( ptr u8 n -- )    CHECK-QUIET-CANDIDATE! 0 T= ;

T-RESET

\ (b) STATIC: a well-typed chooser proposal type-checks; an under-producing or raw-n-in-the-txn-slot
\ chooser is REJECTED, so a raw command / forged txn cannot cross the CHOOSE boundary.
s" AL-CH-OK ( -- n txn ) [: 0 MK-COMMIT-TXN ;] ALOOP:CHOOSE" YES
s" AL-CH-NOTXN ( -- n ) [: 0 ;] ALOOP:CHOOSE" NO
s" AL-CH-RAWN ( -- n n ) [: 0 5 ;] ALOOP:CHOOSE" NO

\ the loop core
AL-PROMOTE -1 T=
AL-PROMOTE-N 1 T=
AL-APPLIC-PROMOTE -1 T=
AL-APPLIC-M 1 T=

\ (c) non-progress + iteration bounds terminate
AL-NONPROG 0 T=
AL-NONPROG-N 3 T=
AL-ITER 1 T=
AL-ITER-N 10 T=

\ (b) registry gate
AL-ILLEGAL-ORD 4 T=
AL-ILLEGAL-ORD-HEAD TTRUE
AL-ILLEGAL-DECL 4 T=

\ (e) authority + budgets
AL-EFF-UNAUTH 3 T=
AL-CAP-UNAUTH 3 T=
AL-CAP-UNAUTH-HEAD TTRUE
AL-EXHAUST 2 T=
AL-EXHAUST-HEAD TTRUE
AL-ATTEN-CAP 3 T=
AL-ATTEN-CAP-HEAD TTRUE
AL-ATTEN-CAP-C 0 T=
AL-ATTEN-BUD 3 T=

\ (d) crash/retry idempotent
AL-IDEMP 60 T=
AL-IDEMP-HEAD TTRUE

\ (a) replay without the chooser
AL-REPLAY TTRUE

\ capacity / fail-closed
' AL-TRK-OVERFLOW E-ALOOP-CAP TTHROWS
' AL-JRN-RANGE E-ALOOP-RANGE TTHROWS

CSTORE:RESET

T-REPORT

;package
