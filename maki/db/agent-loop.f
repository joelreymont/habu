\ maki/db/agent-loop.f - the bounded autonomous agent-loop controller (MODEL-CAD-V2-PLAN.md
\ § 23.2 repair loop "enumerate legal repair actions -> create child revision -> run focused
\ verifier -> compare objective and resource evidence -> promote, continue, or revert" +
\ "repeated non-progress terminates with a typed blocked result instead of an unbounded
\ conversational loop", plan:3255-3272; § 23.9 implementation order item 8 "Agent-loop
\ controller with bounded progress and deterministic replay", plan:3852; dot
\ habu-v2-bounded-autonomous-1c598fcf). This is the capstone that COMPOSES the landed
\ protocol substrate; it forks none of it.
\
\ CONCERN: repeatedly (1) inspect the revision / obligation / diagnostic state, (2) enumerate
\ the registered LEGAL actions (ACTION:ENUM-AT filtered by ACTION:DISPATCH acceptance under the
\ current authority), (3) apply exactly ONE budgeted transaction (CSTORE:COMMIT-AUTHORIZED under
\ an ATTENUATED grant + a budget ledger - the sole mutation path), (4) run focused verification
\ (a typed metric over the tracked obligations' APPLIC:VERDICT), (5) measure progress, and (6)
\ PROMOTE / continue / revert / return a typed BLOCKED result. Package ALOOP. One concern per
\ file: the controller lives here; the action registry (ACTION), the commit store (CSTORE), the
\ capability grant (CAPTOK), the budget ledger (LEDGER), the transaction value (TX), and the
\ obligation applicability checker (APPLIC) are each their own file and are CONSUMED, never forked.
\
\ ---- THE CHOOSER IS UNTRUSTED (the LLM sits behind the action protocol, plan:3200) --------
\ The chooser is a QUOTATION parameter with the checked effect `[ -- n txn ]`: it proposes an
\ action ORDINAL (an index into the canonical ACTION enumeration) and a transaction PAYLOAD. The
\ controller trusts NOTHING it returns - it validates EVERY proposal through the registry / grant
\ / budget gates:
\   - the quotation EFFECT is statically enforced, so an adversarial chooser cannot smuggle a raw
\     command string or forge a `txn` where the type demands one (a raw n cannot stand in the txn
\     slot: nominal safety, proven by the agent-loop-test static verdict fixtures);
\   - an out-of-range ordinal, or one naming a `declared`/unauthorized/wrong-kind action, is
\     REJECTED by ACTION:DISPATCH (unregistered action -> typed blocked, no mutation);
\   - the proposed txn reaches the store ONLY through CSTORE:COMMIT-AUTHORIZED, which re-validates
\     (duplicate-write / omitted-read), authorizes (the attenuated grant), and budgets (the ledger)
\     BEFORE any publish, so an unauthorized or exhausted proposal publishes nothing.
\ The metric quotation `[ -- n ]` is the controller's FOCUSED VERIFIER (the trusted measurement,
\ NOT the untrusted chooser): the canonical instantiation is APPLIC-SATISFIED, which counts the
\ tracked obligations whose APPLIC:VERDICT is `applicable`. The controller never takes the chooser's
\ word for progress; it runs the landed verifier itself.
\
\ ---- BOUNDS (three independent, so a non-progressing or hostile chooser always TERMINATES) ---
\   iteration bound     : MAX-ITERS caps total steps (committed or reverted).
\   non-progress bound  : MAX-NONPROGRESS caps CONSECUTIVE steps that do not raise the metric; a
\                         reverted (conflict/dup/omitted) or idempotent step is non-progress, so a
\                         chooser that never improves the metric hits the bound -> blocked, never
\                         an infinite loop (plan:3271).
\   budget bound        : the LEDGER; an exhausted reserve returns blocked without partial state.
\
\ ---- AUTHORITY (attenuation composes; nested actions cannot exceed the parent) ---------------
\ RUN receives a ROOT grant and, before any mutation, ATTENUATEs it to a CHILD grant from the
\ configured child capability mask + budget ceilings (CAPTOK:ATTENUATE). An escaping request (a
\ cap bit the parent lacks, or a ceiling above the parent's) returns blocked(unauthorized) with
\ NO mutation. Every commit runs under the child grant + the ledger; the action-registry EFFECT
\ authority is the separate CFG-EFF axis DISPATCH gates (the seeded actions' enforced axis).
\
\ ---- DETERMINISTIC REPLAY (the decisions journal re-applies without the chooser, plan:3855) ---
\ Every committed step records the transaction's 32-byte IDEMPOTENCY-KEY into an append-only
\ decisions journal (JOURNAL-COUNT / JOURNAL-KEY@). Because CSTORE:COMMIT-AUTHORIZED is
\ content-addressed and idempotent, REPLAYING the recorded decisions (re-committing the same txns
\ in order, WITHOUT the chooser) reproduces an identical HEAD and ledger digest - and re-running a
\ killed iteration is a no-op (the idempotency-key machinery), so crash/retry is idempotent.
\
\ Typed domain outcomes are the bespoke loop-result sum (the § 23.9 art-result idiom, never
\ value+flag); throws are reserved for capacity / range boundaries. maki -> habu only;
\ agent-loop owns -5389..-5391.

require lib/prelude.f
require maki/db/action.f           \ ACTION: COUNT / ENUM-AT / DISPATCH / EQUAL? : legal-action enumeration + gate
require maki/db/capability.f       \ CAPTOK:grant / ATTENUATE / CAP-MASK@ : the attenuated authority
require maki/db/budget-ledger.f    \ LEDGER:ledger : the budget gate (consumed via COMMIT-AUTHORIZED)
require maki/db/budget-dim.f       \ BUDGET:dim / DIM>N / N>DIM / DIM-COUNT : the shared budget vocabulary
require maki/db/commit-store.f     \ CSTORE:COMMIT-AUTHORIZED : the sole crash-safe, gated mutation path
require maki/db/transaction.f      \ TX:txn / IDEM-KEY>WIRE : the proposed transaction payload
require maki/db/obligation.f       \ OBLIG:obligation : the tracked proof-obligation value
require maki/db/evidence-applicability.f   \ APPLIC:VERDICT : the focused verifier (the metric source)

-5389 constant E-ALOOP-CAP        \ tracked-obligation set over its per-run cap
-5390 constant E-ALOOP-JOURNAL    \ decisions journal over its per-run cap
-5391 constant E-ALOOP-RANGE      \ a journal index outside the recorded range (fail-closed)

package ALOOP
public

\ ---- typed termination reasons (a closed, variant-exhaustive vocabulary) --------
\ The five ways a bounded run stops short of promotion. A closed ENUM, so a MATCH is exhaustive.
ENUM blocked-reason
   nonprogress iter-bound budget-exhausted unauthorized illegal-action
;ENUM

\ ---- the controller outcome (the § 23.9 art-result custom-sum idiom, never value+flag) -------
\ `promoted` = the metric reached the goal; `blocked` names the typed reason the run terminated.
SUMTYPE loop-result 0
   VARIANT promoted ;VARIANT
   VARIANT blocked blocked-reason ;VARIANT
;SUMTYPE

private

\ ---- readable wrappers over the generated constructor spellings ------------------
: LR-PROMOTED ( -- loop-result )                 ALOOP-LOOP--RESULT:PROMOTED ;
: LR-BLOCKED ( blocked-reason -- loop-result )   ALOOP-LOOP--RESULT:BLOCKED ;

\ ---- blocked-reason ordinals (run-state stores the ordinal; -1 means promoted) --
0 constant RN-NONPROGRESS
1 constant RN-ITER-BOUND
2 constant RN-BUDGET
3 constant RN-UNAUTH
4 constant RN-ILLEGAL

: N>REASON ( n -- blocked-reason )               \ fail-closed inverse (an out-of-range ordinal throws)
   case
      RN-NONPROGRESS of ALOOP-BLOCKED--REASON:NONPROGRESS      endof
      RN-ITER-BOUND  of ALOOP-BLOCKED--REASON:ITER-BOUND       endof
      RN-BUDGET      of ALOOP-BLOCKED--REASON:BUDGET-EXHAUSTED endof
      RN-UNAUTH      of ALOOP-BLOCKED--REASON:UNAUTHORIZED     endof
      RN-ILLEGAL     of ALOOP-BLOCKED--REASON:ILLEGAL-ACTION   endof
      E-ALOOP-RANGE throw
   endcase ;

\ ---- per-step outcome codes (PROPOSE-APPLY -> STEP) -----------------------------
0 constant SC-COMMITTED           \ the proposal committed (fresh or idempotent) - a mutation step
1 constant SC-REJECTED            \ conflict / duplicate-write / omitted-read - no mutation, counts as non-progress
2 constant SC-ILLEGAL             \ out-of-range ordinal or DISPATCH unknown/wrong-kind/unsupported (terminal)
3 constant SC-UNAUTH              \ DISPATCH unauthorized OR COMMIT-AUTHORIZED unauthorized (terminal)
4 constant SC-EXHAUSTED           \ COMMIT-AUTHORIZED exhausted budget (terminal)

\ ---- capacities + fixed widths --------------------------------------------------
32 constant TRK-CAP               \ tracked obligations per run (matches APPLIC OBS-CAP)
256 constant JRN-CAP              \ committed decisions per run
32 constant KEY-W                 \ idempotency-key wire width (matches TX:IDEM-KEY>WIRE)

\ ---- configuration (declarative; NO authority handle is stored here - ROOT/LEDGER are RUN
\ parameters, so run state cannot go stale) ---------------------------------------
variable CFG-EFF                                  \ granted action-registry effect mask (the DISPATCH effect axis)
variable CFG-CHILD-CAP                            \ requested child capability mask (for ATTENUATE)
create   CFG-CHILD-BUD BUDGET:DIM-COUNT cells allot   \ requested child budget ceiling vector (for ATTENUATE)
variable CFG-GOAL                                 \ promote when the metric reaches this
variable CFG-MAX-ITERS                            \ iteration bound
variable CFG-MAX-NONPROG                          \ consecutive-non-progress bound
TRK-CAP TYPED-BUFFER TRK-OBL OBLIG:obligation     \ the tracked obligations the metric verifies
variable TRK-N

\ ---- run state (rewritten each RUN) ---------------------------------------------
variable RS-ITER                                  \ steps taken
variable RS-NONPROG                               \ consecutive non-progress steps
variable RS-COMMITS                               \ committed steps (journal length)
variable RS-METRIC                                \ last measured metric
variable RS-DONE                                  \ terminal flag
variable RS-REASON                                \ blocked ordinal, or -1 == promoted

\ ---- decisions journal (append-only idempotency keys) ---------------------------
create JRN-KEYS JRN-CAP KEY-W * allot
variable JRN-N
create KEYSC KEY-W allot                          \ scratch: one idempotency-key wire form
variable SAT-ACC                                  \ APPLIC-SATISFIED accumulator

\ ---- terminal transitions -------------------------------------------------------
: PROMOTE ( -- )        -1 RS-REASON !  true RS-DONE ! ;
: BLOCK ( n -- )        RS-REASON !  true RS-DONE ! ;

\ ---- attenuation: derive the child grant from the configured request ------------
: APPLY-CHILD-BUDGET ( -- )                       \ load the requested ceilings into the CAPTOK builder
   0 begin dup BUDGET:DIM-COUNT < while
      dup {: d:n :}
      d BUDGET:N>DIM  d cells CFG-CHILD-BUD + @  CAPTOK:BUDGET!
      1+
   repeat drop ;

: MK-CHILD ( CAPTOK:grant -- attenuate-result<CAPTOK:grant> ) {: root:CAPTOK:grant :}
   CAPTOK:NEW
   CFG-CHILD-CAP @ CAPTOK:CAP!
   APPLY-CHILD-BUDGET
   root CAPTOK:ATTENUATE ;

\ ---- the untrusted-chooser boundary ---------------------------------------------
\ CHOOSE executes the untrusted chooser quotation and returns its proposal UNVALIDATED (the caller
\ gates it). Its checked effect is exactly the proposal type, so a chooser that does not produce
\ (ordinal, txn) fails the checker - a raw command / a forged raw-n txn cannot cross this boundary.
public
: CHOOSE ( [ -- n txn ] -- n txn ) {: chooser :}   chooser execute ;   \ typed-local-lint: allow-bare-local (quotation local)
private

\ ---- proposal gates -------------------------------------------------------------
: PROPOSAL-LEGAL? ( n -- bool ) {: ord:n :}       \ a registered enumeration ordinal
   ord 0 >=  ord ACTION:COUNT <  and ;

\ GATE authorizes the proposed action through the registry: kind = transaction, effect axis =
\ CFG-EFF, capability axis = the child grant's mask. Returns 0 (accepted), SC-ILLEGAL, or SC-UNAUTH.
: GATE ( CAD-KIND:action-id CAPTOK:grant -- n ) {: id:CAD-KIND:action-id child:CAPTOK:grant :}
   id ACTION-ART--KIND:TRANSACTION  CFG-EFF @  child CAPTOK:CAP-MASK@  ACTION:DISPATCH
   MATCH ACTION:dispatch-result
      accepted       OF 0 ENDOF
      unknown-action OF SC-ILLEGAL ENDOF
      wrong-kind     OF SC-ILLEGAL ENDOF
      unsupported    OF SC-ILLEGAL ENDOF
      unauthorized   OF SC-UNAUTH ENDOF
   ;MATCH ;

\ RECORD-COMMIT appends the committed txn's idempotency key to the decisions journal (fail-closed
\ at capacity), so the run replays deterministically without the chooser.
: RECORD-COMMIT ( txn -- ) {: t:txn :}
   JRN-N @ JRN-CAP >= if E-ALOOP-JOURNAL throw then
   t KEYSC KEY-W TX:IDEM-KEY>WIRE drop
   KEYSC  JRN-N @ KEY-W * JRN-KEYS +  KEY-W BYTE-COPY
   JRN-N @ 1+ JRN-N !
   RS-COMMITS @ 1+ RS-COMMITS ! ;

\ APPLY is the SOLE mutation: the gated, budgeted, crash-safe publish. A fresh or idempotent
\ commit records the decision; a validation reject reverts; unauthorized / exhausted are terminal.
: APPLY ( CAPTOK:grant LEDGER:ledger txn -- n ) {: child:CAPTOK:grant ledger:LEDGER:ledger t:txn :}
   t child ledger CSTORE:COMMIT-AUTHORIZED
   MATCH CSTORE:auth-result
      committed       OF drop t RECORD-COMMIT SC-COMMITTED ENDOF
      conflict        OF SC-REJECTED ENDOF
      duplicate-write OF SC-REJECTED ENDOF
      omitted-read    OF SC-REJECTED ENDOF
      unauthorized    OF SC-UNAUTH ENDOF
      exhausted       OF drop SC-EXHAUSTED ENDOF
   ;MATCH ;

\ PROPOSE-APPLY runs one untrusted proposal through the range check, the registry gate, and the
\ mutation, returning the step code.
: PROPOSE-APPLY ( CAPTOK:grant LEDGER:ledger [ -- n txn ] -- n )
   {: child:CAPTOK:grant ledger:LEDGER:ledger chooser :}   \ typed-local-lint: allow-bare-local (quotation local)
   chooser CHOOSE {: ord:n t:txn :}
   ord PROPOSAL-LEGAL? 0= if SC-ILLEGAL exit then
   ord ACTION:ENUM-AT child GATE {: g:n :}
   g 0<> if g exit then
   child ledger t APPLY ;

\ ---- progress measurement + bounds ----------------------------------------------
: PROGRESS-STEP ( -- )                            \ the metric rose: reset non-progress, promote at goal
   0 RS-NONPROG !
   RS-METRIC @ CFG-GOAL @ >= if PROMOTE then ;

: STALL-STEP ( -- )                               \ the metric did not rise: count it, block at the bound
   RS-NONPROG @ 1+ RS-NONPROG !
   RS-NONPROG @ CFG-MAX-NONPROG @ >= if RN-NONPROGRESS BLOCK then ;

: BUMP-ITER ( -- )                                \ count the step, block at the iteration bound
   RS-ITER @ 1+ RS-ITER !
   RS-ITER @ CFG-MAX-ITERS @ >= if RN-ITER-BOUND BLOCK then ;

: MEASURE ( [ -- n ] -- ) {: metric :}   \ typed-local-lint: allow-bare-local (quotation local)
   metric execute {: m1:n :}
   m1 RS-METRIC @ >  m1 RS-METRIC !               \ ( -- improved? ) then store the new metric
   if PROGRESS-STEP else STALL-STEP then
   RS-DONE @ 0= if BUMP-ITER then ;

\ STEP is one full iteration: propose+apply, map a terminal step code to a blocked reason, else
\ measure progress against the focused verifier.
: STEP ( CAPTOK:grant LEDGER:ledger [ -- n txn ] [ -- n ] -- )
   {: child:CAPTOK:grant ledger:LEDGER:ledger chooser metric :}   \ typed-local-lint: allow-bare-local (quotation locals)
   child ledger chooser PROPOSE-APPLY {: sc:n :}
   sc SC-ILLEGAL   = if RN-ILLEGAL BLOCK exit then
   sc SC-UNAUTH    = if RN-UNAUTH  BLOCK exit then
   sc SC-EXHAUSTED = if RN-BUDGET  BLOCK exit then
   metric MEASURE ;

\ DRIVE seeds the baseline metric (an already-satisfied goal promotes immediately) then iterates
\ under the child grant until a terminal transition sets RS-DONE.
: DRIVE ( CAPTOK:grant LEDGER:ledger [ -- n txn ] [ -- n ] -- )
   {: child:CAPTOK:grant ledger:LEDGER:ledger chooser metric :}   \ typed-local-lint: allow-bare-local (quotation locals)
   metric execute RS-METRIC !
   RS-METRIC @ CFG-GOAL @ >= if PROMOTE exit then
   begin RS-DONE @ 0= while
      child ledger chooser metric STEP
   repeat ;

: RUN-INIT ( -- )
   0 RS-ITER !  0 RS-NONPROG !  0 RS-COMMITS !  0 RS-METRIC !
   0 JRN-N !  false RS-DONE !  -1 RS-REASON ! ;

: RESULT ( -- loop-result )
   RS-REASON @ 0 < if LR-PROMOTED exit then
   RS-REASON @ N>REASON LR-BLOCKED ;

public

\ ---- configuration setters ------------------------------------------------------
: EFFECT-GRANT! ( n -- )                CFG-EFF ! ;
: CHILD-CAP! ( n -- )                   CFG-CHILD-CAP ! ;
: CHILD-BUDGET! ( BUDGET:dim n -- ) {: dim:BUDGET:dim v:n :}
   v dim BUDGET:DIM>N cells CFG-CHILD-BUD + ! ;
: GOAL! ( n -- )                        CFG-GOAL ! ;
: MAX-ITERS! ( n -- )                   CFG-MAX-ITERS ! ;
: MAX-NONPROGRESS! ( n -- )             CFG-MAX-NONPROG ! ;

: TRACK-OBLIGATION+ ( OBLIG:obligation -- ) {: o:OBLIG:obligation :}
   TRK-N @ TRK-CAP >= if E-ALOOP-CAP throw then
   o TRK-N @ TRK-OBL !
   TRK-N @ 1+ TRK-N ! ;

private
: SATISFIED? ( APPLIC:applicability -- bool )
   MATCH APPLIC:applicability
      applicable   OF true ENDOF
      stale        OF false ENDOF
      missing      OF false ENDOF
      inapplicable OF false ENDOF
   ;MATCH ;
public

\ APPLIC-SATISFIED is the canonical focused-verifier metric: the count of tracked obligations whose
\ APPLIC:VERDICT is `applicable` over the current evidence pool. Pass it as `[: ALOOP:APPLIC-SATISFIED ;]`.
: APPLIC-SATISFIED ( -- n )
   0 SAT-ACC !
   0 begin dup TRK-N @ < while
      dup {: k:n :}
      k TRK-OBL @ APPLIC:VERDICT SATISFIED? if 1 SAT-ACC +! then
      1+
   repeat drop
   SAT-ACC @ ;

\ RESET clears the whole controller (config + run state + tracked obligations + journal).
: RESET ( -- )
   0 CFG-EFF !  0 CFG-CHILD-CAP !  0 CFG-GOAL !
   1 CFG-MAX-ITERS !  1 CFG-MAX-NONPROG !
   0 begin dup BUDGET:DIM-COUNT < while  dup cells CFG-CHILD-BUD + 0 swap !  1+  repeat drop
   0 TRK-N !
   RUN-INIT ;

\ RUN drives the untrusted CHOOSER under the ROOT grant (attenuated to a child) and the budget
\ LEDGER, measuring progress with the trusted METRIC verifier, and returns the typed outcome. An
\ attenuation escape is blocked(unauthorized) before any mutation.
: RUN ( CAPTOK:grant LEDGER:ledger [ -- n txn ] [ -- n ] -- loop-result )
   {: root:CAPTOK:grant ledger:LEDGER:ledger chooser metric :}   \ typed-local-lint: allow-bare-local (quotation locals)
   RUN-INIT
   root MK-CHILD
   MATCH CAPTOK:attenuate-result
      ok            OF ledger chooser metric DRIVE ENDOF
      escape-cap    OF RN-UNAUTH BLOCK ENDOF
      escape-budget OF drop RN-UNAUTH BLOCK ENDOF
   ;MATCH
   RESULT ;

\ ---- run-state inspection (read after RUN) --------------------------------------
: ITERATIONS ( -- n )    RS-ITER @ ;
: COMMITS ( -- n )       RS-COMMITS @ ;
: FINAL-METRIC ( -- n )  RS-METRIC @ ;
: NONPROGRESS ( -- n )   RS-NONPROG @ ;

\ ---- decisions journal (the deterministic-replay surface) -----------------------
: JOURNAL-COUNT ( -- n )   JRN-N @ ;
: JOURNAL-KEY@ ( n ptr u8 -- ) {: k:n out:ptr :}   \ copy the k-th committed idempotency key
   k 0 <  k JRN-N @ >=  or if E-ALOOP-RANGE throw then
   k KEY-W * JRN-KEYS +  out  KEY-W BYTE-COPY ;

private
: ALOOP-INIT ( -- )   RESET ;
ALOOP-INIT

;package
