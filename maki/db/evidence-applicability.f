\ maki/db/evidence-applicability.f - obligation closure + evidence applicability
\ (MODEL-CAD-V2-PLAN.md § 23.9 "Proof obligations and independent verifiers", plan:3737-
\ 3755; dot habu-v2-evidence-applicability-73ac58b9).
\
\ CONCERN: over a working set of obligations, an available-evidence pool, and a change-set
\ of artifact ids, decide (a) each obligation's typed APPLICABILITY verdict and (b) the
\ MINIMAL invalidation set a change-set produces. This is a DIFFERENT concern from the
\ obligation VALUE + discharge gate (package OBLIG, maki/db/obligation.f) and the evidence
\ identity registry (package EVIDENCE, maki/db/evidence.f): this file only COMPOSES the
\ landed OBLIG:DISCHARGE and OBLIG:INVALIDATED-BY? primitives into set-level answers.
\
\ ---- APPLICABILITY (per obligation, over the evidence pool + change-set) ---------------
\ The four typed verdicts the dot names (stale / missing / inapplicable) plus the positive
\ complement, each grounded in the landed OBLIG:DISCHARGE reject arms:
\   applicable    - some available evidence DISCHARGES the obligation and no changed
\                   artifact invalidates it.
\   stale         - some evidence discharges it BUT a changed artifact (its subject or a
\                   dependency-cone member) invalidated it (OBLIG:INVALIDATED-BY?).
\   inapplicable  - evidence exists ABOUT this obligation's subject (DISCHARGE rejects with
\                   a NON-subject arm: wrong-domain / wrong-relation / wrong-environment /
\                   wrong-verifier-class / not-independent) but none discharges it. This is
\                   the home of the plan's two structural refusals (plan:3750-3751):
\                   "static proof cannot satisfy required device execution" (a wrong-
\                   verifier-class evidence) and "performance evidence cannot satisfy
\                   equivalence" (a wrong-domain evidence).
\   missing       - NO available evidence is even about this subject (every candidate
\                   DISCHARGE returns wrong-subject: "not linked to the exact subject",
\                   obligation.f), so evidence is simply absent for this obligation.
\
\ ---- CLOSURE (minimal invalidation set) -----------------------------------------------
\ A change-set invalidates EXACTLY the obligations whose subject or dependency cone it
\ touches (OBLIG:INVALIDATED-BY?), never an unrelated one - the "invalidate exactly the
\ affected evidence" rule (plan:3748). The set is computed TWO ways and proven equal:
\   INVALIDATED-SET-UNCACHED - rescans OBLIG:INVALIDATED-BY? for every tracked obligation.
\   INVALIDATED-SET-CACHED   - builds a reverse dependency index once (each referenced
\                   artifact -> the bitmask of obligations naming it as subject or dep) and
\                   unions the masks of the changed artifacts.
\ Both return a u64 bitmask over the OBS slots (OBS-CAP <= 64); CLOSURE-EQUAL? asserts the
\ cache never adds or drops an obligation (a cache-hit closure equals the uncached closure).
\
\ ---- CONSERVATIVE READING (plan under-specifies; grounded in the landed schema) -------
\ The dot frames applicability "over exact subject/dependency/schema/target/numeric/
\ verifier/environment digests". The landed obligation schema (maki/db/obligation.f)
\ realizes those axes as: subject = CAD-KIND:artifact-id; dependency = the artifact-id
\ dependency cone; environment = CAD-KIND:config-id; verifier = the `verifier` class + the
\ producer identity (for independence); numeric = the `domain` proof-domain (the NPOL:dom
\ projection); target/semantics = the `relation` vocabulary. Dedicated schema-id / target-id
\ / numeric-policy-id obligation FIELDS are an additive extension owned by the obligation
\ schema (a new tag + accessor there); this checker composes whatever axes DISCHARGE gates,
\ so it gains them automatically when they land. No new trust boundary. applicability owns
\ -5370.

require lib/prelude.f
require maki/cad-kinds.f
require maki/artifact.f            \ CAD-KIND:artifact-id: EQUAL? (reverse-index key)
require maki/db/obligation.f      \ OBLIG:obligation / evidence / DISCHARGE / INVALIDATED-BY? / SUBJECT@ / DEP@ / DEP-COUNT

-5370 constant E-APPLIC-CAP       \ a tracked obligation / evidence / change / reverse-index set over its cap

package APPLIC
public

\ The four typed applicability verdicts (custom sum, never a value+flag sentinel).
SUMTYPE applicability 0
   VARIANT applicable ;VARIANT
   VARIANT stale ;VARIANT
   VARIANT missing ;VARIANT
   VARIANT inapplicable ;VARIANT
;SUMTYPE

private

\ ---- readable wrappers over the generated constructor spellings ----------------
: APPLICABLE ( -- applicability )    APPLIC-APPLICABILITY:APPLICABLE ;
: STALE ( -- applicability )         APPLIC-APPLICABILITY:STALE ;
: MISSING ( -- applicability )       APPLIC-APPLICABILITY:MISSING ;
: INAPPLICABLE ( -- applicability )  APPLIC-APPLICABILITY:INAPPLICABLE ;

\ ---- working-set capacities (OBS-CAP <= 64 so the invalidation set is one u64) ---------
32 constant OBS-CAP                       \ tracked obligation slots
64 constant EVS-CAP                        \ available evidence slots
32 constant CHG-CAP                        \ change-set artifact ids
320 constant IDX-CAP                        \ distinct referenced artifacts (<= OBS-CAP * (1 + deps))

\ ---- working-set columns -------------------------------------------------------
OBS-CAP TYPED-BUFFER OBS OBLIG:obligation
variable OBS-N
EVS-CAP TYPED-BUFFER EVS OBLIG:evidence
variable EVS-N
CHG-CAP TYPED-BUFFER CHG CAD-KIND:artifact-id
variable CHG-N

\ ---- reverse dependency index (the closure cache) ------------------------------
IDX-CAP TYPED-BUFFER IDX-ART CAD-KIND:artifact-id
create IDX-MASK IDX-CAP cells allot         \ per-distinct-artifact bitmask of referencing OBS slots
variable IDX-N

\ ---- per-VERDICT scan flags (reset per call) -----------------------------------
variable V-DISCHARGED
variable V-SUBJPRES

\ ---- slot access ---------------------------------------------------------------
: OBS@ ( n -- OBLIG:obligation )        OBS @ ;
: EVS@ ( n -- OBLIG:evidence )          EVS @ ;
: CHG@ ( n -- CAD-KIND:artifact-id )    CHG @ ;
: IDX-ART@ ( n -- CAD-KIND:artifact-id ) IDX-ART @ ;
: IDX-MASK@ ( n -- n )                  cells IDX-MASK + @ ;
: IDX-MASK-OR! ( n n -- ) {: bit:n k:n :}   k IDX-MASK@ bit or  k cells IDX-MASK + ! ;

: SLOT-BIT ( n -- n )   1 swap lshift ;    \ the invalidation-set bit for OBS slot n

\ ---- discharge classification (single MATCH over the landed reject arms) --------
0 constant DIS-OK
1 constant DIS-WRONG-SUBJECT              \ evidence is about a DIFFERENT subject
2 constant DIS-OTHER                       \ evidence is about this subject but a non-subject axis rejects

: DISCHARGE-CLASS ( OBLIG:obligation OBLIG:evidence -- n )
   OBLIG:DISCHARGE MATCH OBLIG:discharge-result
      ok                   OF drop DIS-OK ENDOF
      wrong-subject        OF DIS-WRONG-SUBJECT ENDOF
      wrong-domain         OF DIS-OTHER ENDOF
      wrong-relation       OF DIS-OTHER ENDOF
      wrong-environment    OF DIS-OTHER ENDOF
      wrong-verifier-class OF DIS-OTHER ENDOF
      not-independent      OF DIS-OTHER ENDOF
   ;MATCH ;

\ ---- per-obligation invalidation over the change-set ---------------------------
: INVALIDATED? ( OBLIG:obligation -- bool ) {: o:OBLIG:obligation :}
   0 begin dup CHG-N @ < while
      dup {: k:n :}
      o k CHG@ OBLIG:INVALIDATED-BY? if drop true exit then
      1+
   repeat drop false ;

\ ---- evidence-pool scan: is the obligation discharged? is its subject present? --
: VERDICT-SCAN ( OBLIG:obligation -- ) {: o:OBLIG:obligation :}
   false V-DISCHARGED !  false V-SUBJPRES !
   0 begin dup EVS-N @ < while
      dup {: k:n :}
      o k EVS@ DISCHARGE-CLASS {: c:n :}
      c DIS-OK    = if true V-DISCHARGED ! then
      c DIS-OTHER = if true V-SUBJPRES ! then
      1+
   repeat drop ;

public

\ ---- working-set builders ------------------------------------------------------
: RESET ( -- )   0 OBS-N !  0 EVS-N !  0 CHG-N !  0 IDX-N ! ;

: OBLIGATION+ ( OBLIG:obligation -- ) {: o:OBLIG:obligation :}
   OBS-N @ OBS-CAP >= if E-APPLIC-CAP throw then
   o OBS-N @ OBS !
   OBS-N @ 1+ OBS-N ! ;

: EVIDENCE+ ( OBLIG:evidence -- ) {: e:OBLIG:evidence :}
   EVS-N @ EVS-CAP >= if E-APPLIC-CAP throw then
   e EVS-N @ EVS !
   EVS-N @ 1+ EVS-N ! ;

: CHANGE+ ( CAD-KIND:artifact-id -- ) {: a:CAD-KIND:artifact-id :}
   CHG-N @ CHG-CAP >= if E-APPLIC-CAP throw then
   a CHG-N @ CHG !
   CHG-N @ 1+ CHG-N ! ;

: OBLIGATION-COUNT ( -- n )   OBS-N @ ;
: EVIDENCE-COUNT ( -- n )     EVS-N @ ;
: CHANGE-COUNT ( -- n )       CHG-N @ ;

\ ---- APPLICABILITY: the per-obligation verdict ---------------------------------
: VERDICT ( OBLIG:obligation -- applicability ) {: o:OBLIG:obligation :}
   o VERDICT-SCAN
   V-DISCHARGED @ if
      o INVALIDATED? if STALE else APPLICABLE then
   else
      V-SUBJPRES @ if INAPPLICABLE else MISSING then
   then ;

private

\ ---- reverse dependency index (the closure cache) ------------------------------
: IDX-FIND ( CAD-KIND:artifact-id -- n ) {: a:CAD-KIND:artifact-id :}   \ index in IDX-ART, or -1
   IDX-N @ 0 ?do
      a i IDX-ART@ ARTIFACT:EQUAL? if i unloop exit then
   loop -1 ;

: IDX-ADD ( CAD-KIND:artifact-id n -- ) {: a:CAD-KIND:artifact-id s:n :}   \ slot s references artifact a
   a IDX-FIND {: found:n :}
   found 0 >= if
      s SLOT-BIT found IDX-MASK-OR!  exit
   then
   IDX-N @ IDX-CAP >= if E-APPLIC-CAP throw then
   IDX-N @ {: k:n :}
   a k IDX-ART !
   s SLOT-BIT k cells IDX-MASK + !
   k 1+ IDX-N ! ;

: IDX-ADD-DEPS ( OBLIG:obligation n -- ) {: o:OBLIG:obligation s:n :}
   0 begin dup o OBLIG:DEP-COUNT < while
      dup {: d:n :}
      o d OBLIG:DEP@ s IDX-ADD
      1+
   repeat drop ;

: IDX-BUILD ( -- )
   0 IDX-N !
   0 begin dup OBS-N @ < while
      dup {: s:n :}
      s OBS@ {: o:OBLIG:obligation :}
      o OBLIG:SUBJECT@ s IDX-ADD
      o s IDX-ADD-DEPS
      1+
   repeat drop ;

public

\ ---- CLOSURE: the minimal invalidation set, two ways ---------------------------
\ Uncached: rescan OBLIG:INVALIDATED-BY? for every tracked obligation.
: INVALIDATED-SET-UNCACHED ( -- n )   \ u64 bitmask over OBS slots
   0  0 begin dup OBS-N @ < while
      dup {: s:n :}
      s OBS@ INVALIDATED? if s SLOT-BIT rot or swap then
      1+
   repeat drop ;

\ Cached: build the reverse dependency index once, then union the masks of the changed
\ artifacts. Equal to the uncached set because the index maps each of an obligation's
\ (subject + deps) to its bit, which is exactly OBLIG:INVALIDATED-BY?.
: INVALIDATED-SET-CACHED ( -- n )   \ u64 bitmask over OBS slots
   IDX-BUILD
   0  0 begin dup CHG-N @ < while
      dup {: k:n :}
      k CHG@ IDX-FIND {: found:n :}
      found 0 >= if found IDX-MASK@ rot or swap then
      1+
   repeat drop ;

\ CLOSURE-EQUAL? proves the cache-hit closure equals the uncached closure.
: CLOSURE-EQUAL? ( -- bool )   INVALIDATED-SET-UNCACHED INVALIDATED-SET-CACHED = ;

;package
