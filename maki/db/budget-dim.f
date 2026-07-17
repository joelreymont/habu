\ maki/db/budget-dim.f - the closed budget-dimension vocabulary for the autonomy
\ resource contract (MODEL-CAD-V2-PLAN.md § 23 autonomy boundary, plan:3203-3211
\ "experiment, compute, storage, and wall-time budgets"; § 23.1 "resource budgets"
\ plan:3235; dot habu-v2-capability-and-0970a96d). Aligned with
\ habu-v2-types-finite-18bb1b35 (finite closed effect/resource vocabularies).
\
\ CONCERN: ONE closed, variant-exhaustive vocabulary of the budget DIMENSIONS an
\ agent action is metered on. This is the SINGLE source of truth shared by the
\ capability GRANT's budget vector (maki/db/capability.f) and the monotonic budget
\ LEDGER's remaining vector (maki/db/budget-ledger.f), so the two never fork a
\ competing dimension set. It is a DIFFERENT concern from maki/db/action.f's
\ `budget-dim` (which is the ACTION registry's own DECLARED-dimension flag set, a
\ registry field), from the capability TOKEN (package CAPTOK), and from the LEDGER
\ accounting (package LEDGER): those CONSUME this vocabulary, never redefine it.
\
\ THE SIX DIMENSIONS (the plan's autonomy-contract resource axes, plan:3205-3211,
\ realized as the dot's enumerated ledger dimensions): compute-time (host compute),
\ device-time (accelerator wall-time), storage (bytes persisted), candidate-count
\ (search candidates emitted), retries (repair/retry attempts), external-effects
\ (actions requiring external authorization, plan:3210). A closed ENUM DERIVE eq: a
\ MATCH is exhaustive and no out-of-vocabulary dimension is constructible, so a wrong
\ dimension cannot reach a ledger or a grant (the finite-vocabulary discipline).
\
\ DIM>N is the stable little-endian wire/index ordinal (0..5, the vector index a
\ grant/ledger stores against); N>DIM is its fail-closed inverse (an ordinal outside
\ the closed domain throws, never silently maps). maki -> habu only; budget-dim owns
\ -5380.

require lib/prelude.f

-5380 constant E-BUDGET-DIM       \ a dimension ordinal outside the closed 0..5 domain (fail-closed inverse)

package BUDGET
public

\ ---- the closed budget-dimension vocabulary (DERIVE eq for identity compares) ----
ENUM dim DERIVE eq
   compute-time device-time storage candidate-count retries external-effects
;ENUM

6 constant DIM-COUNT              \ vector width: one amount slot per dimension

\ ---- ordinal projection (the stable vector index / wire code) --------------------
: DIM>N ( BUDGET:dim -- n )
   MATCH dim
      compute-time     OF 0 ENDOF
      device-time      OF 1 ENDOF
      storage          OF 2 ENDOF
      candidate-count  OF 3 ENDOF
      retries          OF 4 ENDOF
      external-effects OF 5 ENDOF
   ;MATCH ;

\ ---- fail-closed inverse (an out-of-range ordinal is a wire/decl error, not silent) -
: N>DIM ( n -- BUDGET:dim )
   case
      0 of BUDGET-DIM:COMPUTE-TIME     endof
      1 of BUDGET-DIM:DEVICE-TIME      endof
      2 of BUDGET-DIM:STORAGE          endof
      3 of BUDGET-DIM:CANDIDATE-COUNT  endof
      4 of BUDGET-DIM:RETRIES          endof
      5 of BUDGET-DIM:EXTERNAL-EFFECTS endof
      E-BUDGET-DIM throw
   endcase ;

;package
