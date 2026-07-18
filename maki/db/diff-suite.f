\ maki/db/diff-suite.f - the DifferentialSuite artifact schema (MODEL-CAD-V2-PLAN.md
\ § "Automatic differential verification", plan:3782-3796; dot
\ habu-v2-differential-suite-2d896ced).
\
\ CONCERN: the immutable, content-addressed DIFFERENTIAL-SUITE value plus its canonical
\ encode/digest and the two typed BUILD gates the plan's acceptance names. A suite is a
\ DECLARED ARTIFACT (plan:3784) that names, per plan:3784-3786:
\   - deterministic generators/corpora (content-addressed descriptors),
\   - independent reference implementations (producer-ids, under an independence policy),
\   - normalization, comparison domain/tolerance, metamorphic properties,
\   - target requirements, failure minimizer, seed, and budget.
\ Package DIFFSUITE. One concern per file: the suite VALUE + its encode/digest + the
\ compatibility/independence build gates live here; the isolated RUNNER, reference
\ ADAPTERS, and the counterexample STORE (plan:3787-3796) are separate dots and are
\ CONSUMED, never forked. A durable suite-id registry (the maki/db/obligation.f
\ OBLIGATION-ID leg precedent) and a structured DECODE are deferred follow-ups: the
\ suite's cross-process identity is its DIGEST-INTO content key, which the acceptance
\ (deterministic case-id replay) needs and this file provides.
\
\ ---- FIELD REPRESENTATIONS (grounded in the landed substrate) --------------------
\ generators / metamorphic-properties / references : each a SET of 32-byte content keys,
\   canonicalised ascending + duplicate-free (the maki/db/artifact.f DEPS-CANON set
\   discipline), so the digest is INSERTION-ORDER INDEPENDENT. A generator/property is a
\   deterministic descriptor content-addressed by SHA-256 over the caller's descriptor
\   bytes (GENERATOR+/PROPERTY+ hash locally - the content-addressing mechanism itself,
\   exactly what producer.f/config.f do internally). A reference is a
\   CAD-KIND:producer-id (maki/producer.f owns it) serialised as its cross-process
\   content key (PRODUCER:KEY>WIRE); equal producers share one key, so the reference set
\   is content-addressed and an equal producer cannot appear twice.
\ subject : the CAD-KIND:producer-id of the implementation under test (Habu). Serialised
\   as its PRODUCER:KEY>WIRE content key.
\ normalization / minimizer : each a single 32-byte content key over the caller's
\   descriptor bytes (a deterministic transform / failure-minimizer descriptor).
\ comparison domain : a CAD-KIND:numeric-policy-id (maki/numpolicy.f owns it),
\   serialised as NPOL:KEY>WIRE (8B rank). tolerance : a u64. See the composition note.
\ target needs : a CAD-KIND:target-id (maki/target/target.f owns it), NPOL/PRODUCER
\   style content key (TARGET:KEY>WIRE, 32B).
\ seed : a u64 - the deterministic case-derivation seed (plan:3789 "derives cases
\   deterministically from suite digest and seed").
\ budget : the BUDGET:dim vector (maki/db/budget-dim.f owns the six-dimension
\   vocabulary; DIM-COUNT amount slots indexed by BUDGET:DIM>N) - REUSED, never forked.
\ independence policy : OBLIG:independence {self-verify, independent} (maki/db/
\   obligation.f owns it) - REUSED, never forked (see the composition note).
\
\ ---- COMPARISON DOMAIN/TOLERANCE: COMPOSES WITH NPOL:dom, DOES NOT FORK IT ---------
\ The comparison DOMAIN is exactly an NPOL:dom (held as a CAD-KIND:numeric-policy-id via
\ NPOL:REGISTER; projected back with NPOL:POLICY-DOM). numpolicy deliberately carries NO
\ tolerance value (its enum lattice {exact,ulp,relative,empirical} captures the proof
\ domain). The suite ADDS a numeric tolerance axis, but its COMPATIBILITY with the domain
\ is defined THROUGH NPOL's own semantics, not a competing domain enum: `exact` licenses
\ NO approximation, so it requires a ZERO tolerance; every approximate domain (ulp /
\ relative / empirical) bounds an approximation, so it requires a POSITIVE tolerance. An
\ incompatible (domain, tolerance) pair is a TYPED SEAL reject (tolerance-mismatch), not a
\ silent acceptance - the plan's "no result may mix arithmetic domains silently" boundary
\ (MODEL-CAD-V2-PLAN.md:2804) realised for the differential comparator.
\
\ ---- INDEPENDENCE: COMPOSES WITH THE OBLIG PRODUCER-INDEPENDENCE MODEL --------------
\ Under an `independent` policy a reference implementation may NOT be the subject
\ implementation (plan:3745 "the producer/search pass cannot be the sole verifier for its
\ own claim", realised in maki/db/obligation.f INDEP-VIOLATION? via PRODUCER:EQUAL?). Here
\ the reference-cannot-alias-subject check is CONTENT-KEY inequality of the reference vs
\ the subject producer key - which IS PRODUCER:EQUAL? by content-addressing (equal content
\ key <=> equal interned producer). A `self-verify` policy licenses reference == subject
\ (the positive control). So this schema COMPOSES with OBLIG's independence model over the
\ SAME producer identity; it does not fork a competing independence vocabulary.
\
\ ---- DIGEST: EVERY SEMANTIC FIELD IS DIGEST-COVERED (the envelope precedent) --------
\ DIGEST-INTO is SHA-256 over the canonical semantic prefix (tags 1..12); ENCODE appends
\ the stored digest (tag 13, the only digest-excluded field, as in maki/db/artifact.f).
\ Changing ANY of the twelve semantic fields flips the digest (proven by the DS-test flip
\ matrix). CASE-ID(suite,k) = SHA-256(suite-digest || seed || k), so replaying a suite of
\ identical fields (any insertion order) derives an IDENTICAL case-id sequence and a
\ different seed derives a different one (plan:3789/3796 deterministic replay).
\
\ FIRST-SLICE POOL: a bounded ring of live slots with fixed per-field caps (the maki/db
\ first-slice convention); a durable suite store is a later dot. maki -> habu only;
\ diff-suite owns -5392..-5394.

require lib/prelude.f
require maki/cad-kinds.f
require maki/numpolicy.f           \ NPOL:dom lattice + numeric-policy-id owner (comparison domain)
require maki/producer.f            \ CAD-KIND:producer-id owner (subject + references)
require maki/target/target.f       \ CAD-KIND:target-id owner (target needs)
require maki/db/obligation.f       \ OBLIG:independence policy (reused, not forked)
require maki/db/budget-dim.f       \ BUDGET:dim vector vocabulary (reused, not forked)

-5392 constant E-DSUITE-CAP        \ a per-suite set or the suite pool capacity exceeded
-5393 constant E-DSUITE-BUF        \ ENCODE / DIGEST output buffer smaller than the canonical bytes
-5394 constant E-DSUITE-SEM        \ encode scratch overflow (unreachable internal cap)

package DIFFSUITE
public

\ ---- the suite value handle (opaque PRODUCT over a pooled slot) -----------------
PRODUCT suite 0
   FIELD slot n
;PRODUCT

\ ---- SEAL outcome (custom-sum result, never value+flag) -------------------------
\ ok carries the sealed suite; incomplete rejects a suite missing a required field;
\ tolerance-mismatch and reference-not-independent are the two plan-named typed gates.
SUMTYPE build-result 0
   VARIANT ok suite ;VARIANT
   VARIANT incomplete ;VARIANT
   VARIANT tolerance-mismatch ;VARIANT
   VARIANT reference-not-independent ;VARIANT
;SUMTYPE

private

\ ---- readable wrappers over the generated constructor spellings -----------------
: >SUITE ( n -- suite )   DIFFSUITE-SUITE:MAKE ;
: SUITE> ( suite -- n )   DIFFSUITE-SUITE:UNMAKE ;

: BR-OK ( suite -- build-result )    DIFFSUITE-BUILD--RESULT:OK ;
: BR-INCOMPLETE ( -- build-result )  DIFFSUITE-BUILD--RESULT:INCOMPLETE ;
: BR-TOL ( -- build-result )         DIFFSUITE-BUILD--RESULT:TOLERANCE-MISMATCH ;
: BR-NOINDEP ( -- build-result )     DIFFSUITE-BUILD--RESULT:REFERENCE-NOT-INDEPENDENT ;

\ ---- capacities + fixed widths --------------------------------------------------
32 constant SUITE-CAP             \ live suite slots (ring reuse)
8  constant SET-CAP               \ generators / references / properties per suite
32 constant CKW                   \ content-key width (SHA-256 / owner KEY>WIRE cross-process form)
8  constant U64W                  \ fixed little-endian scalar width
4  constant U32W                  \ fixed little-endian length width
2  constant HDR-W                 \ tag byte + flags byte
1  constant FLAG-REQUIRED         \ flags bit 0
32 constant DIGEST-BYTES          \ suite content digest width
64 constant FID-CAP               \ scratch cap for one foreign-id/content-key wire form
$2000 constant SCRATCH-CAP        \ encode scratch bytes

\ ---- canonical ascending field tags (1..12 digest-covered, 13 excluded) ---------
1  constant TAG-SEED              \ deterministic case-derivation seed (u64)
2  constant TAG-SUBJECT           \ subject producer key (PRODUCER:KEY>WIRE, 32B)
3  constant TAG-POLICY            \ independence policy ordinal (u64)
4  constant TAG-COMPARE           \ comparison domain (NPOL:KEY>WIRE, 8B)
5  constant TAG-TOL               \ comparison tolerance (u64)
6  constant TAG-NORM              \ normalization descriptor content key (32B)
7  constant TAG-MIN               \ failure-minimizer descriptor content key (32B)
8  constant TAG-TARGET            \ target needs (TARGET:KEY>WIRE, 32B)
9  constant TAG-BUDGET            \ budget-dim vector (DIM-COUNT u64 amounts)
10 constant TAG-GEN               \ generators/corpora content-key set
11 constant TAG-REF               \ independent reference producer-key set
12 constant TAG-PROP              \ metamorphic-property content-key set
13 constant TAG-DIGEST            \ stored content digest (excluded from the digest)

\ ---- required-field seen bits (a suite missing one -> incomplete) ---------------
1  constant SEEN-SEED
2  constant SEEN-SUBJECT
4  constant SEEN-POLICY
8  constant SEEN-COMPARE
16 constant SEEN-NORM
32 constant SEEN-MIN
64 constant SEEN-TARGET
127 constant SEEN-ALL              \ all seven required scalar/id fields

\ ---- per-slot columns (parallel; slot-indexed) ----------------------------------
create SEED-COL   SUITE-CAP cells allot
SUITE-CAP TYPED-BUFFER SUBJECT-COL CAD-KIND:producer-id
SUITE-CAP TYPED-BUFFER POLICY-COL  OBLIG:independence
SUITE-CAP TYPED-BUFFER COMPARE-COL CAD-KIND:numeric-policy-id
create TOL-COL    SUITE-CAP cells allot
SUITE-CAP TYPED-BUFFER TARGET-COL  CAD-KIND:target-id
create NORM-COL   SUITE-CAP CKW * allot
create MIN-COL    SUITE-CAP CKW * allot
create BUDGET-COL SUITE-CAP BUDGET:DIM-COUNT * cells allot
create GEN-COL    SUITE-CAP SET-CAP * CKW * allot
create GEN-N-COL  SUITE-CAP cells allot
create REF-COL    SUITE-CAP SET-CAP * CKW * allot
create REF-N-COL  SUITE-CAP cells allot
create PROP-COL   SUITE-CAP SET-CAP * CKW * allot
create PROP-N-COL SUITE-CAP cells allot
variable POOL-NEXT                 \ ring cursor
variable CUR-SLOT                  \ suite slot under construction (NEW)
variable CUR-SEEN                  \ required-field seen bits for the slot under construction

\ ---- scratch buffers ------------------------------------------------------------
create EBUF SCRATCH-CAP allot
variable EO
create DGBUF DIGEST-BYTES allot                \ recomputed suite digest bytes
create KEYSCRATCH FID-CAP allot                \ one foreign-id / descriptor content key
create SUBJKEY CKW allot                       \ subject producer key (independence compare)
create CANON SET-CAP CKW * allot               \ shared set-canonicalisation buffer
create CANON-KEY CKW allot                     \ one-element insertion-sort scratch
variable SORT-I
variable SORT-J
variable DEDUP-W
create CASEBUF DIGEST-BYTES U64W + U64W + allot \ [digest][seed][k] for CASE-ID

\ ---- scalar/id column access (slot-indexed) -------------------------------------
: SEED-R@ ( n -- n )       cells SEED-COL + @ ;
: SEED-R! ( n n -- )       cells SEED-COL + ! ;
: TOL-R@ ( n -- n )        cells TOL-COL + @ ;
: TOL-R! ( n n -- )        cells TOL-COL + ! ;
: SUBJECT-R@ ( n -- CAD-KIND:producer-id )        SUBJECT-COL @ ;
: SUBJECT-R! ( CAD-KIND:producer-id n -- )        SUBJECT-COL ! ;
: POLICY-R@ ( n -- OBLIG:independence )           POLICY-COL @ ;
: POLICY-R! ( OBLIG:independence n -- )           POLICY-COL ! ;
: COMPARE-R@ ( n -- CAD-KIND:numeric-policy-id )  COMPARE-COL @ ;
: COMPARE-R! ( CAD-KIND:numeric-policy-id n -- )  COMPARE-COL ! ;
: TARGET-R@ ( n -- CAD-KIND:target-id )           TARGET-COL @ ;
: TARGET-R! ( CAD-KIND:target-id n -- )           TARGET-COL ! ;
: GEN-N@ ( n -- n )        cells GEN-N-COL + @ ;
: GEN-N! ( n n -- )        cells GEN-N-COL + ! ;
: REF-N@ ( n -- n )        cells REF-N-COL + @ ;
: REF-N! ( n n -- )        cells REF-N-COL + ! ;
: PROP-N@ ( n -- n )       cells PROP-N-COL + @ ;
: PROP-N! ( n n -- )       cells PROP-N-COL + ! ;

\ single-key and set-element byte addresses (32-byte content keys; byte access only)
: NORM-AT ( n -- ptr u8 )   CKW * NORM-COL + ;
: MIN-AT ( n -- ptr u8 )    CKW * MIN-COL + ;
: GEN-AT ( n n -- ptr u8 )  {: s:n k:n :}   s SET-CAP * k + CKW * GEN-COL + ;
: REF-AT ( n n -- ptr u8 )  {: s:n k:n :}   s SET-CAP * k + CKW * REF-COL + ;
: PROP-AT ( n n -- ptr u8 ) {: s:n k:n :}   s SET-CAP * k + CKW * PROP-COL + ;
: BUDGET-CELL ( n n -- a )  {: s:n d:n :}   s BUDGET:DIM-COUNT * d + cells BUDGET-COL + ;

: SLOT-ALLOC ( -- n )   POOL-NEXT @  dup 1+ SUITE-CAP mod POOL-NEXT ! ;

\ ---- fixed little-endian scalar put (byte access) -------------------------------
: LE-PUT ( n ptr u8 n -- ) {: v:n a:ptr w:n :}
   0 begin dup w < while
      dup {: k:n :}
      v k 8 * rshift $FF and  a k + c!
      1+
   repeat drop ;

\ ---- fixed 32-byte content-key comparators (set-canonicalisation ordering) -------
: CKEY-EQ? ( ptr u8 ptr u8 -- bool ) {: pa:ptr pb:ptr :}
   0 begin dup CKW < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

: CKEY-LT? ( ptr u8 ptr u8 -- bool ) {: pa:ptr pb:ptr :}
   0 begin dup CKW < while
      dup {: k:n :}
      pa k + c@  pb k + c@  {: ca:n cb:n :}
      ca cb <> if drop ca cb < exit then
      1+
   repeat drop false ;

\ ---- shared set canonicalisation (ascending, duplicate-free) --------------------
\ One insertion sort + neighbour dedup over the single CANON buffer, reused for all
\ three sets: the caller copies its set into CANON via SET-CANON, which sorts CANON and
\ copies the deduped result back. Only BYTE-COPY touches the caller buffer (no cell
\ access through a passed address), so this is a single sound canonicaliser rather than
\ the per-buffer duplication the ptr-locals rule otherwise forces.
: CANON-AT ( n -- ptr u8 )   CKW * CANON + ;

: CANON-SORT ( n -- ) {: cnt:n :}
   1 SORT-I !
   begin SORT-I @ cnt < while
      SORT-I @ CANON-AT  CANON-KEY  CKW BYTE-COPY
      SORT-I @ SORT-J !
      begin
         SORT-J @ 0 > if  CANON-KEY  SORT-J @ 1- CANON-AT  CKEY-LT?  else false then
      while
         SORT-J @ 1- CANON-AT  SORT-J @ CANON-AT  CKW BYTE-COPY
         SORT-J @ 1- SORT-J !
      repeat
      CANON-KEY  SORT-J @ CANON-AT  CKW BYTE-COPY
      SORT-I @ 1+ SORT-I !
   repeat ;

: CANON-DEDUP ( n -- n ) {: cnt:n :}
   cnt 0= if 0 exit then
   1 DEDUP-W !
   1 SORT-I !
   begin SORT-I @ cnt < while
      SORT-I @ CANON-AT  DEDUP-W @ 1- CANON-AT  CKEY-EQ? 0= if
         SORT-I @ CANON-AT  DEDUP-W @ CANON-AT  CKW BYTE-COPY
         DEDUP-W @ 1+ DEDUP-W !
      then
      SORT-I @ 1+ SORT-I !
   repeat
   DEDUP-W @ ;

: SET-CANON ( ptr u8 n -- n )
   \ typed-local-lint: allow-bare-local - base is a byte-set buffer address, passed only to BYTE-COPY.
   {: base cnt:n :}
   base CANON  cnt CKW *  BYTE-COPY
   cnt CANON-SORT
   cnt CANON-DEDUP {: cnt2:n :}
   CANON base  cnt2 CKW *  BYTE-COPY
   cnt2 ;

: CANON-SETS ( n -- ) {: s:n :}
   s 0 GEN-AT   s GEN-N@   SET-CANON  s GEN-N!
   s 0 REF-AT   s REF-N@   SET-CANON  s REF-N!
   s 0 PROP-AT  s PROP-N@  SET-CANON  s PROP-N! ;

\ ---- semantic-prefix emitter (shared by ENCODE and DIGEST) ----------------------
: E-RESET ( -- )   0 EO ! ;
: E-ROOM ( n -- ) {: k:n :}
   EO @ k + SCRATCH-CAP > if E-DSUITE-SEM throw then ;
: E-U8 ( n -- ) {: c:n :}
   1 E-ROOM  c EBUF EO @ + c!  EO @ 1+ EO ! ;
: E-LE ( n n -- ) {: v:n w:n :}
   w E-ROOM  v EBUF EO @ + w LE-PUT  EO @ w + EO ! ;
: E-BYTES ( ptr u8 n -- ) {: a:ptr u:n :}
   u E-ROOM  a EBUF EO @ + u BYTE-COPY  EO @ u + EO ! ;
: E-HEAD ( n n -- ) {: tag:n flags:n :}   tag E-U8  flags E-U8 ;

\ a length-delimited u64 scalar field: tag, required flag, len=8, then the LE64.
: E-U64-FIELD ( n n -- ) {: tag:n v:n :}
   tag FLAG-REQUIRED E-HEAD
   U64W U32W E-LE
   v U64W E-LE ;

\ a length-delimited content-key field: tag, required flag, len, then the key bytes.
: E-KEY-FIELD ( n ptr u8 n -- ) {: tag:n a:ptr u:n :}
   tag FLAG-REQUIRED E-HEAD
   u U32W E-LE
   a u E-BYTES ;

\ independence policy ordinal (composes with OBLIG:independence; no fork).
: POLICY-ORD ( OBLIG:independence -- n )
   MATCH OBLIG:independence
      self-verify OF 0 ENDOF
      independent OF 1 ENDOF
   ;MATCH ;

\ foreign-id fields: serialise the stored nominal id ACROSS its owner boundary via
\ X:KEY>WIRE (the cross-process content key; total for a valid id), then frame it.
: E-SUBJECT-FIELD ( n -- ) {: s:n :}
   s SUBJECT-R@ KEYSCRATCH FID-CAP PRODUCER:KEY>WIRE {: w:n :}
   TAG-SUBJECT KEYSCRATCH w E-KEY-FIELD ;
: E-COMPARE-FIELD ( n -- ) {: s:n :}
   s COMPARE-R@ KEYSCRATCH FID-CAP NPOL:KEY>WIRE {: w:n :}
   TAG-COMPARE KEYSCRATCH w E-KEY-FIELD ;
: E-TARGET-FIELD ( n -- ) {: s:n :}
   s TARGET-R@ KEYSCRATCH FID-CAP TARGET:KEY>WIRE {: w:n :}
   TAG-TARGET KEYSCRATCH w E-KEY-FIELD ;

\ the budget-dim vector field: tag, flag, len=DIM-COUNT*8, then DIM-COUNT amounts.
: E-BUDGET-FIELD ( n -- ) {: s:n :}
   TAG-BUDGET FLAG-REQUIRED E-HEAD
   BUDGET:DIM-COUNT U64W *  U32W E-LE
   0 begin dup BUDGET:DIM-COUNT < while
      dup {: d:n :}
      s d BUDGET-CELL @  U64W E-LE
      1+
   repeat drop ;

\ a content-key set field: tag, flag, len=count-word + count keys, count, then the keys.
: E-SET-HEAD ( n n -- ) {: tag:n cnt:n :}
   tag FLAG-REQUIRED E-HEAD
   U64W  cnt CKW *  +  U32W E-LE
   cnt U64W E-LE ;
: E-GEN-FIELD ( n -- ) {: s:n :}
   TAG-GEN s GEN-N@ E-SET-HEAD
   0 begin dup s GEN-N@ < while
      dup {: k:n :}   s k GEN-AT CKW E-BYTES   1+
   repeat drop ;
: E-REF-FIELD ( n -- ) {: s:n :}
   TAG-REF s REF-N@ E-SET-HEAD
   0 begin dup s REF-N@ < while
      dup {: k:n :}   s k REF-AT CKW E-BYTES   1+
   repeat drop ;
: E-PROP-FIELD ( n -- ) {: s:n :}
   TAG-PROP s PROP-N@ E-SET-HEAD
   0 begin dup s PROP-N@ < while
      dup {: k:n :}   s k PROP-AT CKW E-BYTES   1+
   repeat drop ;

\ emit the digest-covered semantic fields (tags 1..12) into EBUF, ascending.
: EMIT-SEMANTIC ( n -- ) {: s:n :}
   E-RESET
   TAG-SEED   s SEED-R@                E-U64-FIELD
   s E-SUBJECT-FIELD
   TAG-POLICY s POLICY-R@ POLICY-ORD   E-U64-FIELD
   s E-COMPARE-FIELD
   TAG-TOL    s TOL-R@                 E-U64-FIELD
   TAG-NORM   s NORM-AT CKW            E-KEY-FIELD
   TAG-MIN    s MIN-AT CKW             E-KEY-FIELD
   s E-TARGET-FIELD
   s E-BUDGET-FIELD
   s E-GEN-FIELD
   s E-REF-FIELD
   s E-PROP-FIELD ;

: HASH-SEMANTIC ( n -- )                       \ slot -> EBUF holds tags 1..12, DGBUF = 32 digest bytes
   EMIT-SEMANTIC
   EBUF EO @ DGBUF SHA256 ;

: EMIT-ENVELOPE ( n -- ) {: s:n :}
   s HASH-SEMANTIC                             \ EBUF = tags 1..12, DGBUF = their digest
   TAG-DIGEST FLAG-REQUIRED E-HEAD
   DIGEST-BYTES U32W E-LE
   DGBUF DIGEST-BYTES E-BYTES ;

\ ---- BUILD gates ----------------------------------------------------------------
: MARK-SEEN ( n -- )   CUR-SEEN @ or CUR-SEEN ! ;

: COMPLETE? ( n -- bool ) {: s:n :}
   CUR-SEEN @ SEEN-ALL and SEEN-ALL <> if false exit then
   s GEN-N@ 1 < if false exit then
   s REF-N@ 1 < if false exit then
   true ;

\ tolerance/domain compatibility - composes with NPOL:dom (exact -> zero tolerance;
\ every approximate domain -> positive tolerance). See the header composition note.
: TOL-COMPATIBLE? ( n -- bool ) {: s:n :}
   s COMPARE-R@ NPOL:POLICY-DOM {: d:NPOL:dom :}
   d NPOL-DOM:EXACT NPOL-DOM:EQ if s TOL-R@ 0= else s TOL-R@ 0 > then ;

\ reference-cannot-alias-subject under an independent policy - content-key inequality of
\ each reference vs the subject producer key (== PRODUCER:EQUAL? by content-addressing).
: REF-ALIASES-SUBJ? ( n -- bool ) {: s:n :}
   s SUBJECT-R@ SUBJKEY FID-CAP PRODUCER:KEY>WIRE drop
   0 begin dup s REF-N@ < while
      dup {: k:n :}
      s k REF-AT SUBJKEY CKEY-EQ? if drop true exit then
      1+
   repeat drop false ;

: INDEP-OK? ( n -- bool ) {: s:n :}
   s POLICY-R@ OBLIG-INDEPENDENCE:INDEPENDENT OBLIG-INDEPENDENCE:EQ 0= if true exit then
   s REF-ALIASES-SUBJ? 0= ;

: CLEAR-BUDGET ( n -- ) {: s:n :}
   0 begin dup BUDGET:DIM-COUNT < while
      dup {: d:n :}   0 s d BUDGET-CELL !   1+
   repeat drop ;

public

\ ---- staged suite builder -------------------------------------------------------
\ NEW opens a fresh suite; the typed setters populate the named fields; the `+` builders
\ append the deterministic sets; SEAL validates and returns the immutable handle or a
\ typed reject.
: NEW ( -- )
   SLOT-ALLOC CUR-SLOT !
   0 CUR-SEEN !
   CUR-SLOT @ {: s:n :}
   0 s GEN-N!  0 s REF-N!  0 s PROP-N!
   s CLEAR-BUDGET ;

: SEED! ( n -- )                              CUR-SLOT @ SEED-R!  SEEN-SEED MARK-SEEN ;
: SUBJECT ( CAD-KIND:producer-id -- )         CUR-SLOT @ SUBJECT-R!  SEEN-SUBJECT MARK-SEEN ;
: POLICY ( OBLIG:independence -- )            CUR-SLOT @ POLICY-R!  SEEN-POLICY MARK-SEEN ;
: COMPARISON ( CAD-KIND:numeric-policy-id n -- ) {: id:CAD-KIND:numeric-policy-id tol:n :}
   CUR-SLOT @ {: s:n :}
   id s COMPARE-R!  tol s TOL-R!  SEEN-COMPARE MARK-SEEN ;
: TARGET-NEED ( CAD-KIND:target-id -- )       CUR-SLOT @ TARGET-R!  SEEN-TARGET MARK-SEEN ;

: NORMALIZATION ( ptr u8 n -- ) {: a:ptr u:n :}
   a u  CUR-SLOT @ NORM-AT  SHA256  SEEN-NORM MARK-SEEN ;
: MINIMIZER ( ptr u8 n -- ) {: a:ptr u:n :}
   a u  CUR-SLOT @ MIN-AT  SHA256  SEEN-MIN MARK-SEEN ;

: BUDGET! ( BUDGET:dim n -- ) {: d:BUDGET:dim amt:n :}
   amt  CUR-SLOT @ d BUDGET:DIM>N BUDGET-CELL  ! ;

: GENERATOR+ ( ptr u8 n -- ) {: a:ptr u:n :}
   CUR-SLOT @ {: s:n :}
   s GEN-N@ SET-CAP >= if E-DSUITE-CAP throw then
   a u KEYSCRATCH SHA256
   KEYSCRATCH  s s GEN-N@ GEN-AT  CKW BYTE-COPY
   s GEN-N@ 1+ s GEN-N! ;

: REFERENCE+ ( CAD-KIND:producer-id -- ) {: id:CAD-KIND:producer-id :}
   CUR-SLOT @ {: s:n :}
   s REF-N@ SET-CAP >= if E-DSUITE-CAP throw then
   id KEYSCRATCH FID-CAP PRODUCER:KEY>WIRE drop
   KEYSCRATCH  s s REF-N@ REF-AT  CKW BYTE-COPY
   s REF-N@ 1+ s REF-N! ;

: PROPERTY+ ( ptr u8 n -- ) {: a:ptr u:n :}
   CUR-SLOT @ {: s:n :}
   s PROP-N@ SET-CAP >= if E-DSUITE-CAP throw then
   a u KEYSCRATCH SHA256
   KEYSCRATCH  s s PROP-N@ PROP-AT  CKW BYTE-COPY
   s PROP-N@ 1+ s PROP-N! ;

\ SEAL validates completeness, then the two typed gates (tolerance compatibility, then
\ independence), canonicalises the sets, and returns the immutable suite. Deterministic
\ first-failure order: incomplete -> tolerance-mismatch -> reference-not-independent.
: SEAL ( -- build-result )
   CUR-SLOT @ {: s:n :}
   s COMPLETE? 0= if BR-INCOMPLETE exit then
   s TOL-COMPATIBLE? 0= if BR-TOL exit then
   s INDEP-OK? 0= if BR-NOINDEP exit then
   s CANON-SETS
   s >SUITE BR-OK ;

\ ---- field read surface ---------------------------------------------------------
: SEED@ ( suite -- n )                        SUITE> SEED-R@ ;
: SUBJECT@ ( suite -- CAD-KIND:producer-id )  SUITE> SUBJECT-R@ ;
: POLICY@ ( suite -- OBLIG:independence )     SUITE> POLICY-R@ ;
: COMPARE-DOM@ ( suite -- NPOL:dom )          SUITE> COMPARE-R@ NPOL:POLICY-DOM ;
: TOLERANCE@ ( suite -- n )                   SUITE> TOL-R@ ;
: TARGET@ ( suite -- CAD-KIND:target-id )     SUITE> TARGET-R@ ;
: BUDGET@ ( suite BUDGET:dim -- n ) {: h:suite d:BUDGET:dim :}
   h SUITE> d BUDGET:DIM>N BUDGET-CELL @ ;
: GEN-COUNT ( suite -- n )                    SUITE> GEN-N@ ;
: REF-COUNT ( suite -- n )                    SUITE> REF-N@ ;
: PROP-COUNT ( suite -- n )                   SUITE> PROP-N@ ;
: SUITE-OF ( n -- suite )                     >SUITE ;

\ ---- canonical digest / encode --------------------------------------------------
\ DIGEST-INTO writes the 32-byte SHA-256 over the canonical semantic prefix (the suite's
\ cross-process content key) into the caller buffer. ENCODE writes the full canonical
\ envelope (semantic prefix + stored digest) and returns the byte count.
: DIGEST-INTO ( suite ptr u8 n -- n ) {: h:suite out:ptr cap:n :}
   cap DIGEST-BYTES < if E-DSUITE-BUF throw then
   h SUITE> HASH-SEMANTIC
   DGBUF out DIGEST-BYTES BYTE-COPY
   DIGEST-BYTES ;

: ENCODE ( suite ptr u8 n -- n ) {: h:suite out:ptr cap:n :}
   h SUITE> EMIT-ENVELOPE
   EO @ cap > if E-DSUITE-BUF throw then
   EBUF out EO @ BYTE-COPY
   EO @ ;

\ ---- deterministic case-id derivation (plan:3789) -------------------------------
\ CASE-ID(suite,k) = SHA-256(suite-digest || seed || k). Since the suite digest covers
\ every semantic field (generators included) and the sets are canonical, replaying a
\ suite of identical fields - in any insertion order - derives an identical case-id, and
\ a different seed derives a different one.
: CASE-ID ( suite n ptr u8 -- ) {: h:suite k:n out:ptr :}
   h SUITE> {: s:n :}
   s HASH-SEMANTIC                             \ DGBUF = suite content digest
   DGBUF CASEBUF DIGEST-BYTES BYTE-COPY
   s SEED-R@  CASEBUF DIGEST-BYTES +           U64W LE-PUT
   k          CASEBUF DIGEST-BYTES U64W + +    U64W LE-PUT
   CASEBUF  DIGEST-BYTES U64W + U64W +  out SHA256 ;

private

: DSUITE-INIT ( -- )   0 POOL-NEXT !  0 CUR-SLOT !  0 CUR-SEEN ! ;
DSUITE-INIT

;package
