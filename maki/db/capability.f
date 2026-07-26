\ maki/db/capability.f - the finite, unforgeable capability GRANT token (MODEL-CAD-V2-PLAN.md
\ § 23 autonomy boundary "authority to use ... datasets ... licenses" + "safety constraints
\ and actions that require external authorization", plan:3203-3214; § 23.1 "capability set",
\ "capability tokens ... prevent stale or duplicated agent actions", plan:3219-3235; dot
\ habu-v2-capability-and-0970a96d). Aligned with habu-v2-types-finite-18bb1b35 (finite closed
\ vocabularies + the nominal/refinement discipline).
\
\ CONCERN: the UNFORGEABLE capability grant VALUE and its attenuation algebra. A grant carries
\ (a) a capability-code SET and (b) a budget-ceiling VECTOR over the shared BUDGET dimensions.
\ ATTENUATE derives a CHILD grant that is provably a SUBSET of its parent on BOTH axes, so a
\ delegated (nested) action can never exceed the authority it was handed. This is a DIFFERENT
\ concern from the budget LEDGER accounting (package LEDGER, maki/db/budget-ledger.f, which
\ meters actual usage) and from the ACTION registry's declared authority fields (package
\ ACTION): those CONSUME a grant / share the vocabulary, never redefine it.
\
\ ---- UNFORGEABILITY (raw values cannot forge - the nominal/refinement discipline) --------
\ A grant is a nominal HANDLE (`CAPTOK:grant`, an arity-0 NEWTYPE) over an append-only pool
\ slot that stores the authority. The ONLY authority-bearing public mints are ROOT (the
\ authority ORIGIN - the trusted upstream contract boundary, plan:3203-3211) and ATTENUATE (a
\ subset-only derivation). The private representation refinements RAW>GRANT / GRANT>RAW are
\ owner-only TRUSTED: rows (the maki/db/action.f RAW>ACTION-ID / maki/db/obligation.f
\ RAW>OBLIGATION-ID precedent), confined to this file by tools/refine-lint-core.f + TRUSTED.md.
\ Consequences, each proven by a static checker fixture in the test:
\   - a raw `n` CANNOT stand where a `CAPTOK:grant` is required (nominal safety; verdict 0);
\   - `RAW>GRANT` is unresolvable outside this file (sealed mint; verdict 1);
\   - `=` on a grant rejects (an ADT value never laundry-compares through the scalar prim),
\ so the sole ways to obtain a grant are ROOT and ATTENUATE, and ATTENUATE cannot escalate.
\
\ ---- CAPABILITY CODES ARE OPAQUE BIT FLAGS (the vocabulary content is USER-GATED) --------
\ The capability CODES are held as an opaque u64 BITMASK of closed-vocabulary bits (the plan's
\ "stable constant like the TARGET:CAP-* bits", plan:3652; the maki/db/action.f D-CAP / DISPATCH
\ precedent). The vocabulary CONTENT - which capabilities exist and what each bit means - is a
\ user-gated product decision (owner package CAP, plan:3679 NEEDS-DECISION), NOT invented here:
\ this file treats the mask purely as opaque bits. SUBSET / AUTHORIZES? are mask operations
\ (need AND granted == need), identical to ACTION:DISPATCH's capability gate. When CAP lands,
\ its named bit constructors feed these masks unchanged.
\
\ ---- ATTENUATION IS SUBSET ON BOTH AXES (nested actions cannot exceed parent authority) --
\ A child is admissible iff its requested cap-mask is a SUBSET of the parent's (every requested
\ bit is granted) AND its requested budget ceiling is <= the parent's for EVERY dimension.
\ ATTENUATE returns a bespoke typed result (never value+flag): `ok` carries the child grant;
\ `escape-cap` rejects a cap bit the parent lacks; `escape-budget` names the first dimension
\ whose requested ceiling exceeds the parent's. maki -> habu only; capability owns -5381..-5382.

require lib/prelude.f
require maki/db/budget-dim.f      \ BUDGET:dim / DIM>N / BUDGET:DIM-COUNT: the shared budget-dimension vocabulary

-5381 constant E-CAPTOK-CAP       \ grant pool exhausted (first-slice bounded ring)
-5382 constant E-CAPTOK-RANGE     \ a grant handle outside the allocated slot range (fail-closed)

package CAPTOK
public

\ The unforgeable capability grant: a nominal handle over an append-only authority slot.
NEWTYPE grant 0

\ Typed attenuation outcome (the § 23.9 art-result custom-sum idiom, never value+flag): `ok`
\ carries the derived child grant; the reject arms name the axis that would ESCAPE the parent.
\ Declared through the unified ENUM front end in full mode (the arity token after
\ the name selects it, so the family stays parametric over the grant type). The ok
\ arm names its payload `child` - this file and agent-loop.f's MK-CHILD already
\ call it the CHILD grant - and the escape-budget arm names its dimension cell
\ `dim`, matching maki/db/commit-store.f and the ledger's budget-result. The
\ generated CAPTOK-ATTENUATE--RESULT:OK / :ESCAPE-CAP / :ESCAPE-BUDGET constructors
\ and every MATCH site are unchanged.
ENUM attenuate-result 1
   VARIANT ok FIELD child a ;VARIANT
   VARIANT escape-cap ;VARIANT
   VARIANT escape-budget FIELD dim BUDGET:dim ;VARIANT
;ENUM

private

\ ---- private representation refinement (owner-only, TRUSTED:, refine-lint-confined) ------
\ ROOT / ATTENUATE are the ONLY public mints, each bound to a real slot allocation, so a raw n
\ cannot forge a grant. GRANT>RAW is the owner-only projection used for pooled slot access.
TRUSTED: RAW>GRANT ( n -- CAPTOK:grant ) ;
TRUSTED: GRANT>RAW ( CAPTOK:grant -- n ) ;

\ ---- readable wrappers over the generated constructor spellings ------------------
: AR-OK ( a -- attenuate-result<a> )   CAPTOK-ATTENUATE--RESULT:OK ;
: AR-ESCAPE-CAP ( -- attenuate-result<a> )   CAPTOK-ATTENUATE--RESULT:ESCAPE-CAP ;
: AR-ESCAPE-BUDGET ( BUDGET:dim -- attenuate-result<a> )   CAPTOK-ATTENUATE--RESULT:ESCAPE-BUDGET ;

\ ---- capacities ----------------------------------------------------------------
64 constant GRANT-CAP              \ max live grants (v1 first slice; a durable store is a later dot)

\ ---- authority pool (parallel per-slot columns; the authority lives HERE) --------
create G-CAP GRANT-CAP cells allot                 \ per-slot capability-code bitmask
create G-BUD GRANT-CAP BUDGET:DIM-COUNT * cells allot     \ per-slot budget ceiling vector (BUDGET:DIM-COUNT amounts)
variable G-N                                        \ allocated grant count (append-only)

\ ---- builder scratch (one grant under construction) -----------------------------
variable B-CAP                                      \ requested capability bitmask
create B-BUD BUDGET:DIM-COUNT cells allot                  \ requested budget ceiling vector

\ ---- slot access ---------------------------------------------------------------
: G-CAP@ ( n -- n )   cells G-CAP + @ ;
: G-CAP! ( n n -- ) {: v:n s:n :}   v s cells G-CAP + ! ;
: G-BUD@ ( n n -- n ) {: s:n d:n :}   s BUDGET:DIM-COUNT * d + cells G-BUD + @ ;
: G-BUD! ( n n n -- ) {: v:n s:n d:n :}   v s BUDGET:DIM-COUNT * d + cells G-BUD + ! ;

: SLOT-ALLOC ( -- n )
   G-N @ GRANT-CAP >= if E-CAPTOK-CAP throw then
   G-N @  dup 1+ G-N ! ;

: RANGE-CK ( CAPTOK:grant -- n )                    \ refine a grant to its validated raw slot
   GRANT>RAW dup 0 < over G-N @ >= or if E-CAPTOK-RANGE throw then ;

\ ---- subset predicates ---------------------------------------------------------
: MASK-SUBSET? ( n n -- bool ) {: need:n granted:n :}   need granted and need = ;

\ ---- seal the builder scratch into a fresh authority slot -----------------------
: SEAL ( -- CAPTOK:grant )
   SLOT-ALLOC {: s:n :}
   B-CAP @ s G-CAP!
   0 begin dup BUDGET:DIM-COUNT < while
      dup {: d:n :}
      d cells B-BUD + @  s d G-BUD!
      1+
   repeat drop
   s RAW>GRANT ;

\ First dimension whose requested ceiling exceeds the parent's, or -1 if all fit.
: BUDGET-ESCAPE-DIM ( n -- n ) {: p:n :}            \ p = parent raw slot
   0 begin dup BUDGET:DIM-COUNT < while
      dup {: d:n :}
      d cells B-BUD + @  p d G-BUD@  > if drop d exit then
      1+
   repeat drop -1 ;

public

\ ---- the grant builder ---------------------------------------------------------
\ NEW begins a fresh grant under construction (empty capability set, zero budget ceilings).
\ CAP! sets the requested capability bitmask; BUDGET! sets one dimension's ceiling. ROOT seals
\ an origin grant (the authority-injection boundary); ATTENUATE seals a subset-checked child.
: NEW ( -- )
   0 B-CAP !
   0 begin dup BUDGET:DIM-COUNT < while
      dup cells B-BUD + 0 swap !
      1+
   repeat drop ;

: CAP! ( n -- )   B-CAP ! ;                          \ opaque capability bitmask
: BUDGET! ( BUDGET:dim n -- ) {: dim:BUDGET:dim v:n :}   v dim BUDGET:DIM>N cells B-BUD + ! ;

\ ROOT mints an ORIGIN grant from the builder scratch. This is the trusted authority-injection
\ boundary (the upstream contract provides the root authority, plan:3203-3211); every derived
\ grant flows from ROOT through subset-only ATTENUATE, so authority only ever narrows.
: ROOT ( -- CAPTOK:grant )   SEAL ;

\ ATTENUATE derives a CHILD grant from the builder scratch that must be a SUBSET of `parent` on
\ both axes: escape-cap if a requested capability bit is not granted by the parent; escape-budget
\ naming the first dimension whose requested ceiling exceeds the parent's; else ok(child).
: ATTENUATE ( CAPTOK:grant -- attenuate-result<CAPTOK:grant> ) {: parent:CAPTOK:grant :}
   parent RANGE-CK {: p:n :}
   B-CAP @ p G-CAP@ MASK-SUBSET? 0= if AR-ESCAPE-CAP exit then
   p BUDGET-ESCAPE-DIM {: bad:n :}
   bad 0 >= if bad BUDGET:N>DIM AR-ESCAPE-BUDGET exit then
   SEAL AR-OK ;

\ ---- the grant read surface ----------------------------------------------------
\ VALIDATE fail-closes an out-of-range grant handle; CAP-MASK@ / BUDGET@ read the authority;
\ AUTHORIZES? tests capability containment (grant's set superset of a requested mask, the
\ ACTION:DISPATCH gate); COVERS? tests that the grant's ceiling for a dimension admits `need`.
: VALIDATE ( CAPTOK:grant -- CAPTOK:grant )   dup RANGE-CK drop ;
: CAP-MASK@ ( CAPTOK:grant -- n )   RANGE-CK G-CAP@ ;
: BUDGET@ ( CAPTOK:grant BUDGET:dim -- n ) {: g:CAPTOK:grant dim:BUDGET:dim :}
   g RANGE-CK dim BUDGET:DIM>N G-BUD@ ;
: AUTHORIZES? ( CAPTOK:grant n -- bool ) {: g:CAPTOK:grant need:n :}
   need g CAP-MASK@ MASK-SUBSET? ;
: COVERS? ( CAPTOK:grant BUDGET:dim n -- bool ) {: g:CAPTOK:grant dim:BUDGET:dim need:n :}
   need g dim BUDGET@ <= ;

: COUNT ( -- n )   G-N @ ;

\ RESET clears the whole grant pool (a durable store is a later dot; used by the acceptance tests).
: RESET ( -- )   0 G-N ! ;

;package
