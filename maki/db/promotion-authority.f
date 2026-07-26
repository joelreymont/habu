\ maki/db/promotion-authority.f - the folded obligation-discharge AUTHORITY gate
\ (MODEL-CAD-V2-PLAN.md § 23.9 "independent verifiers" + § 23 autonomy-boundary
\ authorization; dot habu-v2-evidence-promotion-f8312ebe, the discharge-AUTHORITY leg
\ the capbud landing c06b0c14 note assigned here).
\
\ CONCERN: WHO may discharge WHICH obligation, as one typed AUTHORIZED-DISCHARGE gate
\ that FOLDS the three authorization legs the plan separates:
\   1. verifier CLASS         - the obligation's required verifier class must match the
\                               evidence's (maki/db/obligation.f DISCHARGE wrong-verifier-class);
\   2. INDEPENDENCE policy     - under an INDEPENDENT obligation the sole verifier may not be
\                               the producer/claimant (OBLIG:DISCHARGE not-independent);
\   3. verifier IDENTITY       - the discharging verifier's producer-id must be on the
\                               authority's explicit ALLOWLIST (this file's new leg).
\ Legs 1-2 are already the landed OBLIG:DISCHARGE named-field gate; this file COMPOSES it
\ with an identity allowlist. It is the reusable surface CSTORE:COMMIT-AUTHORIZED's missing
\ THIRD leg consumes (its capability + budget legs landed in maki/db/commit-store.f); wiring
\ it into that commit path is a SEPARATE change (the transaction value carries no obligation/
\ evidence discharge context yet, so it is NOT a one-line follow-up - see the dot report).
\
\ UNFORGEABILITY (the CAPTOK:grant discipline). An `authority` is a nominal handle over an
\ append-only allowlist pool sealed by a class-private proof token: SEAL is the only public
\ mint and MINT-AUTH-PROOF is PRIVATE, so a forged authority cannot silently WIDEN who may
\ discharge. The allowlist is populated by the trusted upstream (NEW / AUTHORIZE+ / SEAL, the
\ CAPTOK:ROOT authority-injection precedent).
\
\ maki -> habu only; promotion-authority owns -5607..-5608.

require lib/prelude.f
require maki/db/obligation.f      \ OBLIG: obligation / evidence / DISCHARGE / EV-VERIFIER@
require maki/producer.f           \ CAD-KIND:producer-id EQUAL? (the verifier identity)

-5607 constant E-DAUTH-CAP        \ authority allowlist over its per-authority cap
-5608 constant E-DAUTH-RANGE      \ an authority handle outside the allocated slot range (fail-closed)

package DAUTH
public

\ ---- the sealed authority value (an allowlist of authorized verifier identities) ---
\ Multi-cell (slot + sealed token), so it flows on the stack and is UNMAKEd, never bound
\ as a typed local (the maki/evidence/policy.f multi-cell-product discipline).
TYPEFAMILY auth-proof 0
\ Declared through the unified STRUCTURE front end; the header clause and both FIELD
\ lines are unchanged, so DAUTH-AUTHORITY:MAKE / :UNMAKE keep their exact spelling
\ and effect and no call site moves. It deliberately carries NO `DERIVE eq`, and
\ cannot: the engine refuses a derive clause when any field's own family does not
\ itself derive equality, and an arity-0 TYPEFAMILY proof token never does. That is
\ not a loss here - an authority's meaning is that it EXISTS, minted by the one word
\ allowed to mint it, so equality has nothing to say about it. Comparison of who may
\ discharge goes through the allowlist, not through the handle.
STRUCTURE authority 0
   FIELD slot n
   FIELD tok auth-proof
;STRUCTURE

\ Typed gate outcome (a bespoke custom sum, never value+flag): `ok` carries the accepted
\ evidence; `not-discharged` folds every OBLIG:DISCHARGE named-field refusal (class /
\ independence / subject / domain / relation / environment); `unauthorized` is the new
\ identity leg - the evidence's verifier is not on the authority allowlist.
\ Declared through the unified ENUM front end in full mode (the arity token after the
\ name selects it), so the accepting arm's payload is a named FIELD rather than a
\ positional type token. It carries `ev`, the accepted evidence - the spelling
\ maki/evidence/schema.f already uses for an evidence payload on a full-mode arm, and
\ the value AUTHORIZED-DISCHARGE below binds as `e`. The generated DAUTH-AUTHZ--RESULT:OK
\ / :NOT-DISCHARGED / :UNAUTHORIZED constructors and every MATCH site are unchanged,
\ including the cross-package one in maki/db/commit-store.f: spelling and payload
\ binding order come from the package, the family tail and the declaration order, not
\ from the mode.
ENUM authz-result 0
   VARIANT ok FIELD ev OBLIG:evidence ;VARIANT
   VARIANT not-discharged ;VARIANT
   VARIANT unauthorized ;VARIANT
;ENUM

private
TRUSTED: MINT-AUTH-PROOF ( -- auth-proof )  0 ;

\ ---- readable wrappers over the generated constructor spellings ----------------
: AR-OK ( OBLIG:evidence -- authz-result )   DAUTH-AUTHZ--RESULT:OK ;
: AR-NOTDISCH ( -- authz-result )            DAUTH-AUTHZ--RESULT:NOT-DISCHARGED ;
: AR-UNAUTH ( -- authz-result )              DAUTH-AUTHZ--RESULT:UNAUTHORIZED ;

\ ---- authority allowlist pool (first-slice bounded ring; a durable store is a later dot) ---
16 constant AUTH-CAP              \ live authority slots (ring reuse)
16 constant VERIF-CAP             \ authorized verifier ids per authority

AUTH-CAP VERIF-CAP * TYPED-BUFFER AUTH-VERIF CAD-KIND:producer-id
create AUTH-N AUTH-CAP cells allot          \ per-slot authorized count
variable AUTH-NEXT                            \ ring cursor
variable AUTH-CUR                             \ authority slot under construction (NEW)

: AUTH-N@ ( n -- n )   cells AUTH-N + @ ;
: AUTH-N! ( n n -- ) {: v:n s:n :}   v s cells AUTH-N + ! ;
: VERIF-AT ( n n -- CAD-KIND:producer-id ) {: s:n k:n :}   s VERIF-CAP * k + AUTH-VERIF @ ;
: VERIF-AT! ( CAD-KIND:producer-id n n -- ) {: v:CAD-KIND:producer-id s:n k:n :}
   v  s VERIF-CAP * k + AUTH-VERIF ! ;

: SLOT-ALLOC ( -- n )   AUTH-NEXT @  dup 1+ AUTH-CAP mod AUTH-NEXT ! ;

: SLOT-CK ( n -- n )   dup 0 < over AUTH-CAP >= or if E-DAUTH-RANGE throw then ;

\ VERIFIER-IN-SLOT? scans a validated allowlist slot for a verifier identity.
: VERIFIER-IN-SLOT? ( n CAD-KIND:producer-id -- bool ) {: s:n v:CAD-KIND:producer-id :}
   0 begin dup s AUTH-N@ < while
      dup {: k:n :}
      v s k VERIF-AT PRODUCER:EQUAL? if drop true exit then
      1+
   repeat drop false ;

public

\ ---- the authority builder -----------------------------------------------------
\ NEW opens a fresh (empty) authority; AUTHORIZE+ adds one permitted verifier identity;
\ SEAL mints the immutable handle. The trusted upstream calls these (CAPTOK:ROOT pattern).
: NEW ( -- )   SLOT-ALLOC dup AUTH-CUR !  0 swap AUTH-N! ;
: AUTHORIZE+ ( CAD-KIND:producer-id -- ) {: v:CAD-KIND:producer-id :}
   AUTH-CUR @ {: s:n :}
   s AUTH-N@ {: k:n :}
   k VERIF-CAP >= if E-DAUTH-CAP throw then
   v s k VERIF-AT!  k 1+ s AUTH-N! ;
: SEAL ( -- authority )   AUTH-CUR @ MINT-AUTH-PROOF DAUTH-AUTHORITY:MAKE ;

\ ---- AUTHORIZED-DISCHARGE: the folded three-leg gate ---------------------------
\ Evidence AUTHORIZED-discharges an obligation iff (legs 1-2) it DISCHARGES the obligation
\ AND (leg 3) its verifier identity is on the authority allowlist. Deterministic: the
\ discharge refusal wins over the authorization refusal (a non-discharging evidence never
\ reaches the identity check). The multi-cell authority is UNMAKEd (top) before the
\ single-cell obligation/evidence pop as locals.
: AUTHORIZED-DISCHARGE ( OBLIG:obligation OBLIG:evidence authority -- authz-result )
   DAUTH-AUTHORITY:UNMAKE {: slot:n tok:auth-proof :}
   {: o:OBLIG:obligation e:OBLIG:evidence :}
   slot SLOT-CK drop
   o e OBLIG:DISCHARGE MATCH OBLIG:discharge-result
      ok OF                                  \ the discharged evidence is on the stack (== e)
         slot e OBLIG:EV-VERIFIER@ VERIFIER-IN-SLOT? if AR-OK else drop AR-UNAUTH then
      ENDOF
      wrong-subject        OF AR-NOTDISCH ENDOF
      wrong-domain         OF AR-NOTDISCH ENDOF
      wrong-relation       OF AR-NOTDISCH ENDOF
      wrong-environment    OF AR-NOTDISCH ENDOF
      wrong-verifier-class OF AR-NOTDISCH ENDOF
      not-independent      OF AR-NOTDISCH ENDOF
   ;MATCH ;

: COUNT ( -- n )   AUTH-NEXT @ ;

private

: AUTH-INIT ( -- )   0 AUTH-NEXT !  0 AUTH-CUR ! ;

AUTH-INIT
;package
