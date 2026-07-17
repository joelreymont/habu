\ maki/db/obligation.f - the typed proof-obligation schema (MODEL-CAD-V2-PLAN.md
\ § 23.9 "Proof obligations and independent verifiers" - the prose spec at
\ MODEL-CAD-V2-PLAN.md:3737-3755; dot habu-v2-proof-obligation-6cf70b4f). The
\ obligation-domain / evidence-family realization it composes with is § "Artifact-
\ indexed evidence families" (MODEL-CAD-V2-PLAN.md:1853-1926).
\
\ CONCERN: the immutable proof-OBLIGATION value plus its discharge/invalidation
\ semantics and canonical wire codec. An obligation NAMES (plan:3741) a subject,
\ a claimed relation, a proof domain, a policy, a verifier class, and a required
\ environment; discharge consumes EVIDENCE "linked to the exact subject digest"
\ (plan:3742). Package OBLIG. One concern per file: the value + discharge + wire
\ here; the artifact-indexed evidence FAMILIES (EVID package, maki/evidence/) and
\ the promotion-policy products (POLICY package) are separate dots' concerns and
\ are NOT forked here.
\
\ ---- PROOF DOMAINS: six distinct nominal families (plan:3746) --------------------
\ exact, approximate, empirical, device, safety, and performance are DISTINCT and
\ "non-coercible without a policy constructor" (plan:3747). Realized as one closed
\ `domain` ENUM (DERIVE eq): the six variants are nominally separated, and unlike
\ maki/numpolicy.f's NPOL:dom there is NO strength lattice and NO ordering-based
\ SATISFIES?/COMPOSE - discharge requires domain EQUALITY (DOMAIN-COERCIBLE? is EQ,
\ not <=). Any cross-domain acceptance must go through an explicit policy
\ constructor, which is deliberately absent from this schema (a flagged extension
\ point, below), so the default is fail-closed nominal separation.
\
\ RELATIONSHIP TO NPOL:dom (do not fork a competing lattice - dot brief). NPOL:dom
\ {exact,ulp,relative,empirical} is the NUMERIC-EQUIVALENCE strength lattice: it
\ refines the single "is this arithmetic domain acceptable" axis. The obligation
\ `domain` is COARSER and BROADER - it classifies the KIND of proof that discharges
\ the obligation, of which numeric-equivalence is only one axis. The three numeric
\ families PROJECT from NPOL:dom via the total NPOL>DOMAIN bridge:
\   NPOL exact     -> exact
\   NPOL ulp       -> approximate   (ulp/relative are the two approximate refinements)
\   NPOL relative  -> approximate
\   NPOL empirical -> empirical
\ device/safety/performance have NO numeric-policy source - they are orthogonal proof
\ kinds (device measurement, safety property, performance bound), exactly the axes the
\ plan separates with "a device measurement never proves semantic equivalence"
\ (plan:3751). The bridge is one-way (approximate has two NPOL refinements, so the
\ inverse is not a function); numpolicy's lattice stays the sole authority WITHIN the
\ numeric axis, and this schema does not duplicate it.
\
\ ---- DISCHARGE: the six named-field gate (plan:3745-3751) -------------------------
\ Evidence discharges an obligation iff it matches on every named axis. The typed
\ reject arms make each plan rule a checked outcome:
\   wrong-subject        - evidence not linked to the obligation's exact subject
\   wrong-domain         - wrong proof domain (the non-coercibility rule, plan:3747)
\   wrong-relation       - claims a different relation than the obligation
\   wrong-environment    - produced under a different required environment
\   wrong-verifier-class - wrong verifier class ("static proof never substitutes for
\                          required execution evidence", plan:3750, is a class mismatch)
\   not-independent      - under an INDEPENDENT policy, the sole verifier IS the
\                          producer (plan:3745 "the producer/search pass cannot be the
\                          sole verifier for its own claim")
\ "A device measurement never proves semantic equivalence" needs no extra machinery:
\ a device-domain evidence value fails the domain-equality check against an exact /
\ approximate obligation, so it is structurally unrepresentable as a discharge.
\
\ ---- INVALIDATION: exactly the affected obligations (plan:3748) -------------------
\ Each obligation carries the dependency cone it was discharged over. A changed
\ artifact invalidates an obligation iff it is that obligation's subject or a member
\ of its dependency cone - so an UNRELATED obligation survives an unrelated change
\ (the "invalidate exactly the affected evidence" rule, proven by OB-INVAL-UNRELATED).
\
\ ---- CONSERVATIVE READINGS (plan under-specifies; grounded in landed substrate) ---
\ Flagged at the definition site, following maki/db/diagnostic.f's precedent:
\   - subject-DIGEST: the plan names a 256-bit subject digest, but content-digest is a
\     private multi-cell value owned by package ARTIFACT with no shared nominal. We
\     model `subject` as CAD-KIND:artifact-id (its content-addressed IDENTITY, the
\     strongest identity the repo publishes) - identical to diagnostic.f's reading.
\   - required-ENVIRONMENT: modeled as CAD-KIND:config-id (the environment's
\     configuration identity), the landed identity nearest an environment fingerprint -
\     identical to diagnostic.f's environment-digest reading.
\   - policy: the plan's promotion policy is a large object (POLICY package, out of
\     scope). The obligation's `policy` field is exactly the INDEPENDENCE governance
\     the acceptance rule needs - a closed {self-verify, independent} ENUM - not the
\     whole promotion policy. When POLICY lands, the independent flag becomes a
\     projection of it.
\   - claimed relation / verifier class: the plan does not enumerate these vocabularies.
\     `relation` is grounded in the plan's own "changes semantics, representation,
\     numeric domain, target, deployment/promotion state" list (plan:3739) plus
\     "semantic equivalence" (plan:3751). `verifier` is grounded in the plan's
\     static-vs-execution and device-measurement language (plan:3750). Both are closed
\     ENUMs; extending either is a mechanical additive change with a new ordinal.
\   - PRODUCER (claimant): the plan lists six named fields but the independence rule
\     "for its own claim" (plan:3745) requires the obligation to also name the
\     PROPOSING component. We carry it as CAD-KIND:producer-id (the landed identity),
\     documented as the seventh field the independence rule forces.
\
\ ---- EXTENSION POINT (deliberately absent) ---------------------------------------
\ No policy CONSTRUCTOR that coerces one domain into another exists here. Adding one
\ is the only sanctioned way to make cross-domain evidence acceptable, and it must be
\ an explicit, named, tested boundary (plan:3747 "non-coercible without a policy
\ constructor").
\
\ ---- TRANSACTION REPOINT (item 4): NOT a small mechanical repoint -----------------
\ maki/db/transaction.f holds obligations as opaque u64 identity CODES (TX:OBLIG+,
\ S-OBL). Repointing them to THIS schema needs an obligation IDENTITY with a
\ cross-process wire codec (a CAD-KIND:obligation-id nominal + an owner registry with
\ KEY>WIRE/WIRE>KEY, mirroring maki/producer.f). CAD-KIND:obligation-id is MISSING
\ from the frozen maki/cad-kinds.f, and an interning identity registry is a substantial
\ addition, not a mechanical repoint - so transaction.f is left untouched. Precise
\ follow-up recorded in the dot report and FILEMAP.
\
\ FIRST-SLICE POOL: a bounded ring of live slots with fixed per-field caps (the
\ maki/db/diagnostic.f first-slice convention); a durable obligation store is a later
\ dot. maki -> habu only; obligation owns -5359..-5362.

require lib/prelude.f
require maki/cad-kinds.f
require maki/numpolicy.f          \ NPOL:dom + NPOL:DOM>N: the numeric proof-domain lattice we project from
require maki/artifact.f           \ CAD-KIND:artifact-id owner: REGISTER / KEY$ / EQUAL?
require maki/config.f             \ CAD-KIND:config-id owner: REGISTER / KEY>WIRE / WIRE>KEY / EQUAL?
require maki/producer.f           \ CAD-KIND:producer-id owner: REGISTER / KEY>WIRE / WIRE>KEY / EQUAL?

\ ---- error codes (obligation owns -5359..-5362) -------------------------------
-5359 constant E-OBL-CAP         \ an obligation pool / dependency-cone capacity exceeded
-5360 constant E-OBL-BUF         \ ENCODE output buffer smaller than the canonical bytes
-5361 constant E-OBL-SEM         \ encode scratch overflow (unreachable internal cap)
-5362 constant E-OBL-ORD         \ an enum ordinal outside its closed domain (fail-closed inverse)

package OBLIG
public

\ ---- the six named-field vocabularies (closed, variant-exhaustive ENUMs) -------

\ Proof domain: six distinct families, DERIVE eq, NO coercion lattice (see header).
ENUM domain DERIVE eq
   exact approximate empirical device safety performance
;ENUM

\ Claimed relation: what the obligation asserts about its subject (conservative
\ vocabulary grounded in plan:3739 + plan:3751; see header).
ENUM relation DERIVE eq
   semantic-equiv representation-refine numeric-approx
   target-compat resource-bound safety-property
;ENUM

\ Verifier class: the kind of verifier the obligation requires (conservative
\ vocabulary grounded in plan:3750; see header). static-checker cannot discharge an
\ obligation requiring an execution-class verifier.
ENUM verifier DERIVE eq
   static-checker differential-exec golden-recompute
   device-measure safety-prover perf-benchmark
;ENUM

\ Policy: the independence governance (see header). Under `independent` the sole
\ verifier may not be the producer. Named `independence` because `policy` is a
\ reserved type-declaration keyword (src/core/sumtype.f layout-policy header); the
\ obligation's plan-named FIELD is still `POLICY` / `POLICY@`.
ENUM independence DERIVE eq
   self-verify independent
;ENUM

\ ---- the value handles (opaque PRODUCTs over pooled slots) ---------------------
PRODUCT obligation 0
   FIELD slot n
;PRODUCT

PRODUCT evidence 0
   FIELD slot n
;PRODUCT

\ ---- discharge / decode outcomes (custom-sum results, never value+flag) --------
\ ok carries the accepted evidence; every reject arm is one plan rule.
SUMTYPE discharge-result 0
   VARIANT ok evidence ;VARIANT
   VARIANT wrong-subject ;VARIANT
   VARIANT wrong-domain ;VARIANT
   VARIANT wrong-relation ;VARIANT
   VARIANT wrong-environment ;VARIANT
   VARIANT wrong-verifier-class ;VARIANT
   VARIANT not-independent ;VARIANT
;SUMTYPE

SUMTYPE decode-result 0
   VARIANT ok obligation ;VARIANT
   VARIANT malformed ;VARIANT
   VARIANT noncanonical ;VARIANT
   VARIANT bounds ;VARIANT
   VARIANT duplicate ;VARIANT
   VARIANT unknown-required ;VARIANT
;SUMTYPE

private

\ ---- readable wrappers over the generated constructor spellings ----------------
: >OBL ( n -- obligation )   OBLIG-OBLIGATION:MAKE ;
: OBL> ( obligation -- n )   OBLIG-OBLIGATION:UNMAKE ;
: >EV ( n -- evidence )      OBLIG-EVIDENCE:MAKE ;
: EV> ( evidence -- n )      OBLIG-EVIDENCE:UNMAKE ;

: DR-OK ( evidence -- discharge-result )    OBLIG-DISCHARGE--RESULT:OK ;
: DR-WSUBJECT ( -- discharge-result )       OBLIG-DISCHARGE--RESULT:WRONG-SUBJECT ;
: DR-WDOMAIN ( -- discharge-result )        OBLIG-DISCHARGE--RESULT:WRONG-DOMAIN ;
: DR-WRELATION ( -- discharge-result )      OBLIG-DISCHARGE--RESULT:WRONG-RELATION ;
: DR-WENV ( -- discharge-result )           OBLIG-DISCHARGE--RESULT:WRONG-ENVIRONMENT ;
: DR-WVCLASS ( -- discharge-result )        OBLIG-DISCHARGE--RESULT:WRONG-VERIFIER-CLASS ;
: DR-NOINDEP ( -- discharge-result )        OBLIG-DISCHARGE--RESULT:NOT-INDEPENDENT ;

: DC-OK ( n -- decode-result )              >OBL OBLIG-DECODE--RESULT:OK ;
: DC-MALFORMED ( -- decode-result )         OBLIG-DECODE--RESULT:MALFORMED ;
: DC-NONCANON ( -- decode-result )          OBLIG-DECODE--RESULT:NONCANONICAL ;
: DC-BOUNDS ( -- decode-result )            OBLIG-DECODE--RESULT:BOUNDS ;
: DC-DUP ( -- decode-result )               OBLIG-DECODE--RESULT:DUPLICATE ;
: DC-UNKNOWN ( -- decode-result )           OBLIG-DECODE--RESULT:UNKNOWN-REQUIRED ;

\ ---- ordinal <-> enum bridges (stable wire ids; inverses fail closed) ----------
6 constant DOMAIN-VARIANTS
6 constant RELATION-VARIANTS
6 constant VERIFIER-VARIANTS
2 constant POLICY-VARIANTS

: DOMAIN>N ( domain -- n )
   MATCH domain
      exact       OF 0 ENDOF
      approximate OF 1 ENDOF
      empirical   OF 2 ENDOF
      device      OF 3 ENDOF
      safety      OF 4 ENDOF
      performance OF 5 ENDOF
   ;MATCH ;

: N>DOMAIN ( n -- domain )
   case
      0 of OBLIG-DOMAIN:EXACT       endof
      1 of OBLIG-DOMAIN:APPROXIMATE endof
      2 of OBLIG-DOMAIN:EMPIRICAL   endof
      3 of OBLIG-DOMAIN:DEVICE      endof
      4 of OBLIG-DOMAIN:SAFETY      endof
      5 of OBLIG-DOMAIN:PERFORMANCE endof
      E-OBL-ORD throw
   endcase ;

: RELATION>N ( relation -- n )
   MATCH relation
      semantic-equiv        OF 0 ENDOF
      representation-refine OF 1 ENDOF
      numeric-approx        OF 2 ENDOF
      target-compat         OF 3 ENDOF
      resource-bound        OF 4 ENDOF
      safety-property       OF 5 ENDOF
   ;MATCH ;

: N>RELATION ( n -- relation )
   case
      0 of OBLIG-RELATION:SEMANTIC-EQUIV        endof
      1 of OBLIG-RELATION:REPRESENTATION-REFINE endof
      2 of OBLIG-RELATION:NUMERIC-APPROX        endof
      3 of OBLIG-RELATION:TARGET-COMPAT         endof
      4 of OBLIG-RELATION:RESOURCE-BOUND        endof
      5 of OBLIG-RELATION:SAFETY-PROPERTY       endof
      E-OBL-ORD throw
   endcase ;

: VERIFIER>N ( verifier -- n )
   MATCH verifier
      static-checker    OF 0 ENDOF
      differential-exec OF 1 ENDOF
      golden-recompute  OF 2 ENDOF
      device-measure    OF 3 ENDOF
      safety-prover     OF 4 ENDOF
      perf-benchmark    OF 5 ENDOF
   ;MATCH ;

: N>VERIFIER ( n -- verifier )
   case
      0 of OBLIG-VERIFIER:STATIC-CHECKER    endof
      1 of OBLIG-VERIFIER:DIFFERENTIAL-EXEC endof
      2 of OBLIG-VERIFIER:GOLDEN-RECOMPUTE  endof
      3 of OBLIG-VERIFIER:DEVICE-MEASURE    endof
      4 of OBLIG-VERIFIER:SAFETY-PROVER     endof
      5 of OBLIG-VERIFIER:PERF-BENCHMARK    endof
      E-OBL-ORD throw
   endcase ;

: POLICY>N ( independence -- n )
   MATCH independence
      self-verify OF 0 ENDOF
      independent OF 1 ENDOF
   ;MATCH ;

: N>POLICY ( n -- independence )
   case
      0 of OBLIG-INDEPENDENCE:SELF-VERIFY endof
      1 of OBLIG-INDEPENDENCE:INDEPENDENT endof
      E-OBL-ORD throw
   endcase ;

public

\ ---- NPOL bridge (the documented, one-way projection; see header) --------------
\ Total over NPOL:dom's four ranks (0..3 = exact/ulp/relative/empirical); ulp and
\ relative both project to `approximate`. device/safety/performance have no numeric
\ source, so there is no inverse function.
: NPOL>DOMAIN ( NPOL:dom -- domain )
   NPOL:DOM>N case
      0 of OBLIG-DOMAIN:EXACT       endof   \ NPOL exact
      1 of OBLIG-DOMAIN:APPROXIMATE endof   \ NPOL ulp
      2 of OBLIG-DOMAIN:APPROXIMATE endof   \ NPOL relative
      3 of OBLIG-DOMAIN:EMPIRICAL   endof   \ NPOL empirical
      E-OBL-ORD throw
   endcase ;

\ Non-coercibility as a checked predicate: `have` evidence-domain may discharge a
\ `need` obligation-domain iff they are EQUAL. Contrast maki/numpolicy.f SATISFIES?
\ (a lattice <=): obligation domains do NOT order, so nothing coerces without a
\ policy constructor.
: DOMAIN-COERCIBLE? ( domain domain -- bool )   OBLIG-DOMAIN:EQ ;

private

\ ---- pool capacities (first-slice bounded ring) -------------------------------
16 constant OB-CAP               \ live obligation slots (ring reuse)
16 constant EV-CAP               \ live evidence slots (ring reuse)
8  constant DEP-CAP              \ dependency-cone artifact ids per obligation
256 constant ART-KEY-MAX         \ max artifact store-key bytes (matches ARTIFACT ART-KEY-MAX)

\ ---- protocol constants (frozen wire) -----------------------------------------
8  constant U64W                 \ fixed little-endian scalar width
4  constant U32W                 \ fixed little-endian length width
2  constant HDR-W                \ tag byte + flags byte
1  constant FLAG-REQUIRED        \ flags bit 0
32 constant CK-BYTES             \ owner cross-process content-key width (CONFIG/PRODUCER KEY>WIRE)
64 constant FID-WIRE-CAP         \ scratch cap for one foreign-id wire form
$2000 constant SCRATCH-CAP       \ encode scratch bytes

\ Canonical ascending field tags; all eight are required-present.
1 constant TAG-SUBJECT           \ CAD-KIND:artifact-id KEY$ (content-addressed)
2 constant TAG-RELATION          \ relation ordinal (u64)
3 constant TAG-DOMAIN            \ domain ordinal (u64)
4 constant TAG-POLICY            \ policy ordinal (u64)
5 constant TAG-VCLASS            \ verifier ordinal (u64)
6 constant TAG-ENV               \ CAD-KIND:config-id (CONFIG:KEY>WIRE, 32B content key)
7 constant TAG-PRODUCER          \ CAD-KIND:producer-id (PRODUCER:KEY>WIRE, 32B content key)
8 constant TAG-DEPS              \ dependency cone: CAD-KIND:artifact-id KEY$ list
8 constant TAG-KNOWN-MAX

\ taxonomy codes accumulated during decode, mapped to decode-result at the boundary
1 constant D-MALFORMED
2 constant D-NONCANON
3 constant D-BOUNDS
4 constant D-DUP
5 constant D-UNKNOWN-REQ

\ ---- obligation pool (parallel per-slot columns) ------------------------------
OB-CAP TYPED-BUFFER OB-SUBJECT-COL  CAD-KIND:artifact-id
OB-CAP TYPED-BUFFER OB-RELATION-COL relation
OB-CAP TYPED-BUFFER OB-DOMAIN-COL   domain
OB-CAP TYPED-BUFFER OB-POLICY-COL   independence
OB-CAP TYPED-BUFFER OB-VCLASS-COL   verifier
OB-CAP TYPED-BUFFER OB-ENV-COL      CAD-KIND:config-id
OB-CAP TYPED-BUFFER OB-PRODUCER-COL CAD-KIND:producer-id
OB-CAP DEP-CAP * TYPED-BUFFER OB-DEP-COL CAD-KIND:artifact-id
create OB-DEP-N OB-CAP cells allot
variable OB-NEXT                 \ ring cursor
variable OB-CUR                  \ obligation slot under construction (NEW)

\ ---- evidence pool (parallel per-slot columns) --------------------------------
EV-CAP TYPED-BUFFER EV-SUBJECT-COL  CAD-KIND:artifact-id
EV-CAP TYPED-BUFFER EV-DOMAIN-COL   domain
EV-CAP TYPED-BUFFER EV-RELATION-COL relation
EV-CAP TYPED-BUFFER EV-ENV-COL      CAD-KIND:config-id
EV-CAP TYPED-BUFFER EV-VERIFIER-COL CAD-KIND:producer-id
EV-CAP TYPED-BUFFER EV-VCLASS-COL   verifier
variable EV-NEXT                 \ ring cursor

\ ---- scratch encode buffer + cursor -------------------------------------------
create EBUF SCRATCH-CAP allot
variable EO
create IDWBUF FID-WIRE-CAP allot          \ one foreign-id wire form

\ ---- decode cursor state ------------------------------------------------------
PTR-VARIABLE DBASE
variable DLEN
variable DPOS
variable DPREV
variable DERR
variable SEEN
variable DI

\ ---- obligation column access (slot-indexed) ----------------------------------
: OB-SUBJECT-R@ ( n -- CAD-KIND:artifact-id )   OB-SUBJECT-COL @ ;
: OB-SUBJECT-R! ( CAD-KIND:artifact-id n -- )   OB-SUBJECT-COL ! ;
: OB-RELATION-R@ ( n -- relation )              OB-RELATION-COL @ ;
: OB-RELATION-R! ( relation n -- )              OB-RELATION-COL ! ;
: OB-DOMAIN-R@ ( n -- domain )                  OB-DOMAIN-COL @ ;
: OB-DOMAIN-R! ( domain n -- )                  OB-DOMAIN-COL ! ;
: OB-POLICY-R@ ( n -- independence )            OB-POLICY-COL @ ;
: OB-POLICY-R! ( independence n -- )            OB-POLICY-COL ! ;
: OB-VCLASS-R@ ( n -- verifier )                OB-VCLASS-COL @ ;
: OB-VCLASS-R! ( verifier n -- )                OB-VCLASS-COL ! ;
: OB-ENV-R@ ( n -- CAD-KIND:config-id )         OB-ENV-COL @ ;
: OB-ENV-R! ( CAD-KIND:config-id n -- )         OB-ENV-COL ! ;
: OB-PRODUCER-R@ ( n -- CAD-KIND:producer-id )  OB-PRODUCER-COL @ ;
: OB-PRODUCER-R! ( CAD-KIND:producer-id n -- )  OB-PRODUCER-COL ! ;
: OB-DEP-N-R@ ( n -- n )   cells OB-DEP-N + @ ;
: OB-DEP-N-R! ( n n -- )   cells OB-DEP-N + ! ;
: OB-DEP-AT ( n n -- CAD-KIND:artifact-id ) {: s:n k:n :}   s DEP-CAP * k + OB-DEP-COL @ ;
: OB-DEP-AT! ( CAD-KIND:artifact-id n n -- ) {: v:CAD-KIND:artifact-id s:n k:n :}
   v  s DEP-CAP * k + OB-DEP-COL ! ;

\ ---- evidence column access (slot-indexed) ------------------------------------
: EV-SUBJECT-R@ ( n -- CAD-KIND:artifact-id )   EV-SUBJECT-COL @ ;
: EV-SUBJECT-R! ( CAD-KIND:artifact-id n -- )   EV-SUBJECT-COL ! ;
: EV-DOMAIN-R@ ( n -- domain )                  EV-DOMAIN-COL @ ;
: EV-DOMAIN-R! ( domain n -- )                  EV-DOMAIN-COL ! ;
: EV-RELATION-R@ ( n -- relation )              EV-RELATION-COL @ ;
: EV-RELATION-R! ( relation n -- )              EV-RELATION-COL ! ;
: EV-ENV-R@ ( n -- CAD-KIND:config-id )         EV-ENV-COL @ ;
: EV-ENV-R! ( CAD-KIND:config-id n -- )         EV-ENV-COL ! ;
: EV-VERIFIER-R@ ( n -- CAD-KIND:producer-id )  EV-VERIFIER-COL @ ;
: EV-VERIFIER-R! ( CAD-KIND:producer-id n -- )  EV-VERIFIER-COL ! ;
: EV-VCLASS-R@ ( n -- verifier )                EV-VCLASS-COL @ ;
: EV-VCLASS-R! ( verifier n -- )                EV-VCLASS-COL ! ;

\ ---- slot allocation ----------------------------------------------------------
: OB-SLOT-ALLOC ( -- n )   OB-NEXT @  dup 1+ OB-CAP mod OB-NEXT ! ;
: EV-SLOT-ALLOC ( -- n )   EV-NEXT @  dup 1+ EV-CAP mod EV-NEXT ! ;

: OB-SLOT-RESET ( n -- )   0 swap OB-DEP-N-R! ;

\ ---- fixed little-endian scalar codec -----------------------------------------
: LE-PUT ( n ptr u8 n -- ) {: v:n a:ptr w:n :}
   0 begin dup w < while
      dup {: k:n :}
      v k 8 * rshift $FF and  a k + c!
      1+
   repeat drop ;

: LE-GET ( ptr u8 n -- n ) {: a:ptr w:n :}
   0  0 begin dup w < while
      dup {: k:n :}
      a k + c@ k 8 * lshift  rot or swap
      1+
   repeat drop ;

\ ---- semantic-prefix emitter --------------------------------------------------
: E-RESET ( -- )   0 EO ! ;
: E-ROOM ( n -- ) {: k:n :}
   EO @ k + SCRATCH-CAP > if E-OBL-SEM throw then ;
: E-U8 ( n -- ) {: c:n :}
   1 E-ROOM  c EBUF EO @ + c!  EO @ 1+ EO ! ;
: E-LE ( n n -- ) {: v:n w:n :}
   w E-ROOM  v EBUF EO @ + w LE-PUT  EO @ w + EO ! ;
: E-BYTES ( ptr u8 n -- ) {: a:ptr u:n :}
   u E-ROOM  a EBUF EO @ + u BYTE-COPY  EO @ u + EO ! ;
: E-HEAD ( n n -- ) {: tag:n flags:n :}   tag E-U8  flags E-U8 ;

\ A length-delimited u64 scalar field: tag, required flag, len=8, then the LE64.
: E-U64-FIELD ( n n -- ) {: tag:n v:n :}
   tag FLAG-REQUIRED E-HEAD
   U64W U32W E-LE
   v U64W E-LE ;

\ A length-delimited byte field: tag, required flag, len, then the bytes. Serves
\ artifact KEY$ payloads and foreign-id KEY>WIRE content keys alike.
: E-STR-FIELD ( n ptr u8 n -- ) {: tag:n a:ptr u:n :}
   tag FLAG-REQUIRED E-HEAD
   u U32W E-LE
   a u E-BYTES ;

\ subject: the stored artifact-id serialized as its content-addressed store KEY$.
: E-SUBJECT-FIELD ( n -- ) {: s:n :}
   s OB-SUBJECT-R@ ARTIFACT:KEY$ {: a:ptr u:n :}
   TAG-SUBJECT a u E-STR-FIELD ;

\ foreign ids across the owner boundary via KEY>WIRE (cross-process content key).
: E-ENV-FIELD ( n -- ) {: s:n :}
   s OB-ENV-R@ IDWBUF FID-WIRE-CAP CONFIG:KEY>WIRE {: w:n :}
   TAG-ENV IDWBUF w E-STR-FIELD ;
: E-PRODUCER-FIELD ( n -- ) {: s:n :}
   s OB-PRODUCER-R@ IDWBUF FID-WIRE-CAP PRODUCER:KEY>WIRE {: w:n :}
   TAG-PRODUCER IDWBUF w E-STR-FIELD ;

\ dependency cone: count then each element's artifact KEY$ (u32 len + bytes),
\ insertion order preserved (a byte-identical round-trip). When the obligation
\ gains a content-key identity (the deferred transaction repoint), canonicalise
\ this set ascending/dup-free like maki/db/artifact.f DEPS-CANON.
: E-DEPS-FIELD ( n -- ) {: s:n :}
   TAG-DEPS FLAG-REQUIRED E-HEAD
   s OB-DEP-N-R@ {: cnt:n :}
   \ backpatch is avoided: the payload is a length prefix computed at emit time is
   \ awkward for variable strings, so reserve+patch. Reserve the u32, remember its
   \ offset, emit, then patch.
   EO @ {: off:n :}   0 U32W E-LE
   cnt U64W E-LE
   0 begin dup cnt < while
      dup {: k:n :}
      s k OB-DEP-AT ARTIFACT:KEY$ {: a:ptr u:n :}
      u U32W E-LE  a u E-BYTES
      1+
   repeat drop
   EO @ off U32W + -  EBUF off + U32W LE-PUT ;

: EMIT-ENVELOPE ( n -- ) {: s:n :}
   E-RESET
   s E-SUBJECT-FIELD
   TAG-RELATION s OB-RELATION-R@ RELATION>N E-U64-FIELD
   TAG-DOMAIN   s OB-DOMAIN-R@   DOMAIN>N   E-U64-FIELD
   TAG-POLICY   s OB-POLICY-R@   POLICY>N   E-U64-FIELD
   TAG-VCLASS   s OB-VCLASS-R@   VERIFIER>N E-U64-FIELD
   s E-ENV-FIELD
   s E-PRODUCER-FIELD
   s E-DEPS-FIELD ;

\ ---- decode helpers -----------------------------------------------------------
: FAIL ( n -- )      DERR ! ;
: FAILED? ( -- bool ) DERR @ 0<> ;
: REMAIN ( -- n )    DLEN @ DPOS @ - ;

: D-U8 ( -- n )
   REMAIN 1 < if D-MALFORMED FAIL 0 exit then
   DBASE 0 ptr-field @ DPOS @ + c@
   DPOS @ 1+ DPOS ! ;

: D-LE ( n -- n ) {: w:n :}
   REMAIN w < if D-MALFORMED FAIL 0 exit then
   DBASE 0 ptr-field @ DPOS @ + w LE-GET
   DPOS @ w + DPOS ! ;

\ take a span of `len` bytes at the cursor (fail-closed on short input).
: D-TAKE ( n -- ptr u8 n ) {: len:n :}
   DBASE 0 ptr-field @ {: base:ptr :}
   REMAIN len < if D-MALFORMED FAIL base 0 exit then
   base DPOS @ + {: sp:ptr :}
   DPOS @ len + DPOS !
   sp len ;

: SEEN-CLEAR ( -- )   0 SEEN ! ;
: SEEN-TAG ( n -- )   1 swap lshift SEEN @ or SEEN ! ;
: SEEN? ( n -- bool ) 1 swap lshift SEEN @ and 0<> ;

\ ---- required-field bodies ----------------------------------------------------
: TAKE-U64 ( n -- n ) {: declen:n :}
   declen U64W <> if D-BOUNDS FAIL 0 exit then
   U64W D-LE ;

: TAKE-ENUM ( n n -- n ) {: declen:n hi:n :}     \ scalar u64 ordinal, range-checked
   declen TAKE-U64 {: ord:n :}
   FAILED? if 0 exit then
   ord 0 < ord hi >= or if D-BOUNDS FAIL 0 exit then
   ord ;

\ subject: content-addressed artifact key bytes, re-interned by content. A missing
\ or over-long key is a typed reject (not a throw), tightening diagnostic.f's reading.
: TAKE-SUBJECT ( n n -- ) {: s:n declen:n :}
   declen D-TAKE {: a:ptr u:n :}
   FAILED? if exit then
   u 0= if D-MALFORMED FAIL exit then
   u ART-KEY-MAX > if D-BOUNDS FAIL exit then
   a u ARTIFACT:REGISTER s OB-SUBJECT-R! ;

\ foreign ids: resolve the content key fail-closed through the owner WIRE>KEY BY
\ CONTENT, folding its reject arms into the taxonomy (wrong-width->malformed,
\ unknown->bounds), exactly the maki/db/artifact.f contract.
: TAKE-ENV ( n n -- ) {: s:n declen:n :}
   declen D-TAKE {: a:ptr u:n :}
   FAILED? if exit then
   a u CONFIG:WIRE>KEY
   MATCH CONFIG:id-result
      ok          OF s OB-ENV-R! ENDOF
      wrong-width OF D-MALFORMED FAIL ENDOF
      unknown     OF D-BOUNDS FAIL ENDOF
   ;MATCH ;

: TAKE-PRODUCER ( n n -- ) {: s:n declen:n :}
   declen D-TAKE {: a:ptr u:n :}
   FAILED? if exit then
   a u PRODUCER:WIRE>KEY
   MATCH PRODUCER:id-result
      ok          OF s OB-PRODUCER-R! ENDOF
      wrong-width OF D-MALFORMED FAIL ENDOF
      unknown     OF D-BOUNDS FAIL ENDOF
   ;MATCH ;

: TAKE-DEPS ( n n -- ) {: s:n declen:n :}
   declen U64W < if D-BOUNDS FAIL exit then
   U64W D-LE {: cnt:n :}
   FAILED? if exit then
   cnt 0 < cnt DEP-CAP > or if D-BOUNDS FAIL exit then
   0 s OB-DEP-N-R!
   0 begin dup cnt < FAILED? 0= and while
      dup {: k:n :}
      U32W D-LE {: klen:n :}
      FAILED? if drop exit then
      klen 0= if D-MALFORMED FAIL drop exit then
      klen ART-KEY-MAX > if D-BOUNDS FAIL drop exit then
      klen D-TAKE {: a:ptr u:n :}
      FAILED? 0= if a u ARTIFACT:REGISTER s k OB-DEP-AT!  k 1+ s OB-DEP-N-R! then
      1+
   repeat drop ;

: KNOWN-BODY ( n n n -- ) {: s:n tag:n declen:n :}
   tag SEEN? if D-DUP FAIL exit then
   tag SEEN-TAG
   tag TAG-SUBJECT  = if s declen TAKE-SUBJECT  exit then
   tag TAG-RELATION = if declen RELATION-VARIANTS TAKE-ENUM N>RELATION s OB-RELATION-R!  exit then
   tag TAG-DOMAIN   = if declen DOMAIN-VARIANTS TAKE-ENUM N>DOMAIN s OB-DOMAIN-R!  exit then
   tag TAG-POLICY   = if declen POLICY-VARIANTS TAKE-ENUM N>POLICY s OB-POLICY-R!  exit then
   tag TAG-VCLASS   = if declen VERIFIER-VARIANTS TAKE-ENUM N>VERIFIER s OB-VCLASS-R!  exit then
   tag TAG-ENV      = if s declen TAKE-ENV  exit then
   tag TAG-PRODUCER = if s declen TAKE-PRODUCER  exit then
   tag TAG-DEPS     = if s declen TAKE-DEPS  exit then ;

: FIELD-STEP ( n -- ) {: s:n :}
   D-U8 {: tag:n :}    FAILED? if exit then
   D-U8 {: flags:n :}  FAILED? if exit then
   U32W D-LE {: declen:n :}  FAILED? if exit then
   declen 0 < if D-BOUNDS FAIL exit then
   tag DPREV @ = if D-DUP FAIL exit then
   tag DPREV @ < if D-NONCANON FAIL exit then
   tag DPREV !
   tag TAG-KNOWN-MAX <= tag 0 > and if
      s tag declen KNOWN-BODY exit
   then
   flags FLAG-REQUIRED and 0<> if
      REMAIN declen < if D-MALFORMED FAIL exit then
      D-UNKNOWN-REQ FAIL exit
   then
   declen D-TAKE 2drop ;             \ skip an unknown OPTIONAL field

: REQUIRED-COMPLETE? ( -- bool )
   TAG-SUBJECT SEEN? TAG-RELATION SEEN? and TAG-DOMAIN SEEN? and
   TAG-POLICY SEEN? and TAG-VCLASS SEEN? and TAG-ENV SEEN? and
   TAG-PRODUCER SEEN? and TAG-DEPS SEEN? and ;

: DEC-SETUP ( ptr u8 n -- ) {: a:ptr u:n :}
   a DBASE 0 ptr-field !  u DLEN !  0 DPOS !  -1 DPREV !  0 DERR !
   SEEN-CLEAR ;

: DEC-SLOT-FILL ( n -- ) {: s:n :}
   s OB-SLOT-RESET
   begin REMAIN 0 > FAILED? 0= and while
      s FIELD-STEP
   repeat ;

: DEC-RESULT ( n -- decode-result ) {: s:n :}
   DERR @ 0= if s DC-OK exit then
   DERR @ D-MALFORMED = if DC-MALFORMED exit then
   DERR @ D-NONCANON  = if DC-NONCANON exit then
   DERR @ D-BOUNDS    = if DC-BOUNDS exit then
   DERR @ D-DUP       = if DC-DUP exit then
   DC-UNKNOWN ;

public

\ ---- staged obligation builder ------------------------------------------------
\ NEW opens a fresh obligation; the typed setters populate the six named fields plus
\ the producer (claimant) and the dependency cone; SEAL returns the immutable handle.
: NEW ( -- )                          OB-SLOT-ALLOC dup OB-CUR ! OB-SLOT-RESET ;
: SUBJECT ( CAD-KIND:artifact-id -- )     OB-CUR @ OB-SUBJECT-R! ;
: RELATION ( relation -- )                OB-CUR @ OB-RELATION-R! ;
: DOMAIN ( domain -- )                    OB-CUR @ OB-DOMAIN-R! ;
: POLICY ( independence -- )              OB-CUR @ OB-POLICY-R! ;
: VERIFIER-CLASS ( verifier -- )          OB-CUR @ OB-VCLASS-R! ;
: ENVIRONMENT ( CAD-KIND:config-id -- )   OB-CUR @ OB-ENV-R! ;
: PRODUCER ( CAD-KIND:producer-id -- )    OB-CUR @ OB-PRODUCER-R! ;
: DEP+ ( CAD-KIND:artifact-id -- ) {: id:CAD-KIND:artifact-id :}
   OB-CUR @ {: s:n :}
   s OB-DEP-N-R@ {: k:n :}
   k DEP-CAP >= if E-OBL-CAP throw then
   id s k OB-DEP-AT!  k 1+ s OB-DEP-N-R! ;
: SEAL ( -- obligation )              OB-CUR @ >OBL ;

\ ---- evidence constructor -----------------------------------------------------
\ Evidence is "linked to the exact subject digest" (plan:3742) - it names the subject,
\ its own proof domain / claimed relation / environment / verifier-class, and the
\ verifier IDENTITY (producer-id) that produced it.
: EVIDENCE ( CAD-KIND:artifact-id domain relation CAD-KIND:config-id CAD-KIND:producer-id verifier -- evidence )
   {: subj:CAD-KIND:artifact-id dom:domain rel:relation env:CAD-KIND:config-id
      ver:CAD-KIND:producer-id vc:verifier :}
   EV-SLOT-ALLOC {: s:n :}
   subj s EV-SUBJECT-R!  dom s EV-DOMAIN-R!  rel s EV-RELATION-R!
   env s EV-ENV-R!  ver s EV-VERIFIER-R!  vc s EV-VCLASS-R!
   s >EV ;

\ ---- obligation field accessors (the value read surface) ----------------------
: SUBJECT@ ( obligation -- CAD-KIND:artifact-id )   OBL> OB-SUBJECT-R@ ;
: RELATION@ ( obligation -- relation )              OBL> OB-RELATION-R@ ;
: DOMAIN@ ( obligation -- domain )                  OBL> OB-DOMAIN-R@ ;
: POLICY@ ( obligation -- independence )            OBL> OB-POLICY-R@ ;
: VERIFIER-CLASS@ ( obligation -- verifier )        OBL> OB-VCLASS-R@ ;
: ENVIRONMENT@ ( obligation -- CAD-KIND:config-id ) OBL> OB-ENV-R@ ;
: PRODUCER@ ( obligation -- CAD-KIND:producer-id )  OBL> OB-PRODUCER-R@ ;
: DEP-COUNT ( obligation -- n )                     OBL> OB-DEP-N-R@ ;
: DEP@ ( obligation n -- CAD-KIND:artifact-id ) {: o:obligation k:n :}   o OBL> k OB-DEP-AT ;

\ ---- evidence field accessors -------------------------------------------------
: EV-SUBJECT@ ( evidence -- CAD-KIND:artifact-id )   EV> EV-SUBJECT-R@ ;
: EV-DOMAIN@ ( evidence -- domain )                  EV> EV-DOMAIN-R@ ;
: EV-RELATION@ ( evidence -- relation )              EV> EV-RELATION-R@ ;
: EV-ENVIRONMENT@ ( evidence -- CAD-KIND:config-id ) EV> EV-ENV-R@ ;
: EV-VERIFIER@ ( evidence -- CAD-KIND:producer-id )  EV> EV-VERIFIER-R@ ;
: EV-VERIFIER-CLASS@ ( evidence -- verifier )        EV> EV-VCLASS-R@ ;

private

\ Independence: under an INDEPENDENT policy the sole verifier must not be the
\ producer (plan:3745). self-verify licenses producer == verifier.
: INDEP-VIOLATION? ( obligation evidence -- bool ) {: o:obligation e:evidence :}
   o POLICY@ OBLIG-INDEPENDENCE:INDEPENDENT OBLIG-INDEPENDENCE:EQ 0= if false exit then
   e EV-VERIFIER@ o PRODUCER@ PRODUCER:EQUAL? ;

public

\ ---- DISCHARGE: the named-field gate (see header) -----------------------------
\ Evidence discharges an obligation iff it matches on subject, domain, relation,
\ environment, and verifier-class, and does not violate the independence policy.
\ Checks run structural-first and return the FIRST failing axis (deterministic).
: DISCHARGE ( obligation evidence -- discharge-result ) {: o:obligation e:evidence :}
   e EV-SUBJECT@ o SUBJECT@ ARTIFACT:EQUAL? 0= if DR-WSUBJECT exit then
   e EV-DOMAIN@ o DOMAIN@ DOMAIN-COERCIBLE? 0= if DR-WDOMAIN exit then
   e EV-RELATION@ o RELATION@ OBLIG-RELATION:EQ 0= if DR-WRELATION exit then
   e EV-ENVIRONMENT@ o ENVIRONMENT@ CONFIG:EQUAL? 0= if DR-WENV exit then
   e EV-VERIFIER-CLASS@ o VERIFIER-CLASS@ OBLIG-VERIFIER:EQ 0= if DR-WVCLASS exit then
   o e INDEP-VIOLATION? if DR-NOINDEP exit then
   e DR-OK ;

\ ---- INVALIDATION: exactly the affected obligations (plan:3748) ---------------
\ A changed artifact invalidates an obligation iff it is its subject or a member of
\ its dependency cone; an unrelated obligation survives an unrelated change.
: INVALIDATED-BY? ( obligation CAD-KIND:artifact-id -- bool )
   {: o:obligation changed:CAD-KIND:artifact-id :}
   changed o SUBJECT@ ARTIFACT:EQUAL? if true exit then
   0 begin dup o DEP-COUNT < while
      dup {: k:n :}
      changed o k DEP@ ARTIFACT:EQUAL? if drop true exit then
      1+
   repeat drop false ;

\ ---- canonical wire codec -----------------------------------------------------
: ENCODE ( obligation ptr u8 n -- n ) {: o:obligation out:ptr cap:n :}
   o OBL> EMIT-ENVELOPE
   EO @ cap > if E-OBL-BUF throw then
   EBUF out EO @ BYTE-COPY
   EO @ ;

: DECODE ( ptr u8 n -- decode-result ) {: a:ptr u:n :}
   a u DEC-SETUP
   OB-SLOT-ALLOC {: s:n :}
   s DEC-SLOT-FILL
   FAILED? 0= REQUIRED-COMPLETE? 0= and if D-MALFORMED FAIL then
   s DEC-RESULT ;

\ OBL-OF rebuilds an obligation handle from a validated ok slot (the maki/db/
\ artifact.f WEIGHT-OF precedent).
: OBL-OF ( n -- obligation )   >OBL ;

private

: OBL-CODEC-INIT ( -- )
   0 OB-NEXT !  0 EV-NEXT !  0 OB-CUR ! ;

OBL-CODEC-INIT
;package
