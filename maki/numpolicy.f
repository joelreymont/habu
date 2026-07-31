\ maki/numpolicy.f - the typed numeric-policy proof-domain family (docs/archive/cad-plan.md §A3 +
\ section 22.6: "Precision is part of the plan and artifact key. A conversion or
\ approximate rewrite requires evidence in one of the exact, ULP, relative-error,
\ or empirically licensed domains").
\
\ V1 already tags every op with a RAW numeric class in maki/op-registry.f
\ (NUM-EXACT / NUM-ULP / NUM-RELTOL). Those are bare ints: nothing stops a
\ relative-tolerance TF32 tensor-core result from being compared against an exact
\ FP32-FMA reference under the same plan/artifact key (the motivating confusion,
\ MODEL-CAD-V2-PLAN.md:2804 "No result may mix arithmetic domains silently").
\
\ This file promotes that raw tag to a TYPED proof domain `dom` and gives it the
\ two operations the plan's Exit line requires:
\   - SATISFIES? / ENFORCE: an approximate candidate CANNOT satisfy a stricter
\     policy (E-NPOL-APPROX is the named checked refusal);
\   - COMPOSE: composing domains across a pipeline yields the WEAKEST domain
\     deterministically (a total-order max - commutative, associative, idempotent).
\ The domain also becomes part of the EXACT schedule/plan key: maki/sched-key.f
\ REGION-POL folds each region's ops into its REQUESTED policy and renders it into
\ SK-KEY / SK-KEY$, so a different op mix (a different honest policy) invalidates
\ plan/artifact/tuning key lookups (different policy => different key => no baseline
\ pairing), and the typed golden record carries the achieved domain (schema.f).
\
\ The requested policy is DERIVED PER-OP, not an ambient per-class knob: REGION-POL
\ folds each op's INTRINSIC numeric domain (OP-DOM below) over the region with
\ COMPOSE (weakest wins), so the region's requested proof domain is an HONEST
\ function of its ops - a pure-relu region requests `exact`, a pure-transcendental
\ elementwise region (only gelu/silu) requests `relative`, and a region fusing a
\ matmul/row-reduce/decode op requests `relative` too. This closes the "exact-and-
\ always-refusing" trap without a per-class table: elementwise is a MIXED class
\ (exact relu/cast, ulp add/mul, transcendental gelu/silu), so no single class
\ default is honest for it - per-OP is. The IDENTICAL fold on the ACHIEVED side is
\ cad.f REGION-ACHIEVED (requested vs achieved stay distinct axes: the promote gate
\ composes the golden's judged precision into achieved, then ENFORCEs it satisfies
\ the requested policy). The bridge NUM>DOM / OP-DOM projects an op's raw registry
\ numeric class into the domain lattice (empirical has no per-op source - it is a
\ golden/recompute-level license, so NUM>DOM never yields it and fails closed on a
\ NUM-N tag).
\
\ Fail closed: an out-of-range wire id or raw numeric tag is a named throw.
\ maki -> habu only; numpolicy owns -5145..-5147.

require lib/prelude.f
require lib/errors.f
require maki/cad-kinds.f            \ CAD-KIND:numeric-policy-id nominal identity
require maki/op-registry.f          \ NUM-* raw numeric class, OPR-NUMERIC

-5145 constant E-NPOL-DOM       \ numeric-policy wire/ordinal id out of range
-5146 constant E-NPOL-APPROX    \ approximate evidence cannot satisfy a stricter numeric policy
-5147 constant E-NPOL-WIRE      \ ID>WIRE output buffer smaller than the fixed wire width

package NPOL
public

\ ---- the proof domain, ordered by STRENGTH (exact strongest .. empirical weakest) ----
\ DERIVE eq gives NPOL-DOM:EQ so `dom` can be an enum FIELD of the typed schedule
\ key (maki/sched-key.f skey) and the typed golden record (maki/evidence/schema.f).
\ Variants, strongest first (inline `\` notes inside an ENUM block are a parse error):
\   exact     - bit-exact / FP32 FMA reference: no approximation licensed
\   ulp       - ULP-bounded arithmetic
\   relative  - relative-error bounded (transcendental / accumulated / TF32 tensor-core)
\   empirical - empirically licensed against an independent golden dataset (recompute)
ENUM dom DERIVE eq
   exact
   ulp
   relative
   empirical
;ENUM

\ ---- strength lattice ---------------------------------------------------------
\ RANK is the single named ordinal boundary the lattice folds on; it doubles as the
\ stable wire/table id (exact=0 .. empirical=3). Strength DECREASES as rank rises.
: RANK ( dom -- n )
   MATCH dom
      exact     OF 0 ENDOF
      ulp       OF 1 ENDOF
      relative  OF 2 ENDOF
      empirical OF 3 ENDOF
   ;MATCH ;

\ SATISFIES?: does `have`-domain evidence meet the `need` policy? Evidence is
\ acceptable iff it is at least as STRONG as the requirement (rank <=). Exact
\ evidence satisfies anything; empirical evidence satisfies only an empirical need.
: SATISFIES? ( dom dom -- bool ) {: have:dom need:dom :}
   have RANK  need RANK  <= ;

\ COMPOSE: pipeline composition - the WEAKER (higher-rank) domain wins. A total-order
\ max, so composing a pipeline is deterministic, commutative, associative, idempotent
\ (e.g. TF32 relative matmul + exact elementwise => relative).
: COMPOSE ( dom dom -- dom ) {: a:dom b:dom :}
   a RANK  b RANK  >=  if a else b then ;

\ ENFORCE: the checked satisfaction GATE - a named refusal unless `have` evidence
\ satisfies the `need` policy. This is the "approximate candidate cannot satisfy an
\ exact policy" boundary; a golden/promotion carrying a relative or empirical result
\ throws E-NPOL-APPROX against an exact requirement.
: ENFORCE ( dom dom -- )  SATISFIES? 0= if E-NPOL-APPROX throw then ;

\ ---- durable key token (bare, positional pipe-field in SK-KEY$) ---------------
: NAME ( dom -- ptr u8 n )
   MATCH dom
      exact     OF s" exact" ENDOF
      ulp       OF s" ulp"   ENDOF
      relative  OF s" rel"   ENDOF
      empirical OF s" emp"   ENDOF
   ;MATCH ;

\ ---- wire projection (store/durable ordinal id; inverse fails closed) ----------
: DOM>N ( dom -- n )  RANK ;
: N>DOM ( n -- dom )
   case
      0 of NPOL-DOM:EXACT     endof
      1 of NPOL-DOM:ULP       endof
      2 of NPOL-DOM:RELATIVE  endof
      3 of NPOL-DOM:EMPIRICAL endof
      E-NPOL-DOM throw
   endcase ;

\ ---- bridge from op-registry's raw per-op numeric class -----------------------
\ NUM-EXACT/NUM-ULP/NUM-RELTOL project into the domain lattice; empirical has no
\ per-op source (it is a golden/recompute-level license), so a NUM-N tag fails closed.
: NUM>DOM ( n -- dom )
   case
      MAKI:NUM-EXACT  of NPOL-DOM:EXACT    endof
      MAKI:NUM-ULP    of NPOL-DOM:ULP      endof
      MAKI:NUM-RELTOL of NPOL-DOM:RELATIVE endof
      E-NPOL-DOM throw
   endcase ;
: OP-DOM ( MAKI:opkind -- dom )  MAKI:OPR-NUMERIC NUM>DOM ;   \ an op's numeric domain (achieved AND, folded by REGION-POL, requested)

\ ---- § 23.9 numeric-policy-id: audited nominal identity + wire codec -----------
\ ENGINEERING DECISION - numeric-policy descriptor scope (resolved in dot
\ habu-npol-numeric-policy-a90657e1 from real usage): a numeric policy is a SINGLE
\ proof `dom`, NOT a per-region map or tolerance bundle. Every maki consumer binds
\ exactly one dom - sched-key.f REGION-POL ( region -- dom ) and its `FIELD pol
\ NPOL:dom`, cad.f REGION-ACHIEVED ( region -- dom ), evidence/schema.f `FIELD pol
\ NPOL:dom`, evidence/policy.f `FIELD npol NPOL:dom` and GOLD-VERDICT's `need` dom -
\ and no numeric tolerance value exists anywhere (the enum lattice
\ {exact,ulp,relative,empirical} captures it). The region->policy association lives in
\ sched-key's per-region key, not in a policy bundle. So the identity's content is its
\ single dom, and DOM>N / N>DOM (rank 0..3) is the exact content key the plan names as
\ the minimal precedent (MODEL-CAD-V2-PLAN.md § 23.9 numeric-policy-id row).

\ WIRE>ID decode result (the maki/db/artifact.f art-result custom-sum idiom): `ok`
\ carries the refined nominal id in its `id` field; the reject arms are the
\ fixed-width byte-decode refusals. A bespoke per-package sum, not result<a,b>, so a
\ total ok construction leaves no free error variable. Declared through the unified
\ ENUM front end in full mode (the arity after the name selects it), so the payload is
\ a named FIELD rather than a positional one; the generated NPOL-ID--RESULT:OK /
\ :WRONG-WIDTH / :UNKNOWN constructors and every MATCH site are unchanged, because both
\ spellings and payload binding order derive from the package, the family tail, and the
\ declaration order, none of which the mode changes.
ENUM id-result<a>
   VARIANT ok FIELD id a ;VARIANT
   VARIANT wrong-width ;VARIANT
   VARIANT unknown ;VARIANT
;ENUM

private

4 constant DOM-COUNT            \ closed dom cardinality (exact..empirical); matches N>DOM
8 constant WIRE-BYTES           \ fixed little-endian wire width of the registry raw

\ Private representation refinements (owner-only, TRUSTED:, never public), the
\ maki/target/target.f RAW>TARGET-ID / TARGET-ID>RAW precedent. The raw IS the dom
\ rank (0..3), so an id is content-addressed by its proof domain.
TRUSTED: RAW>NUMERIC-POLICY-ID ( n -- CAD-KIND:numeric-policy-id ) ;
TRUSTED: NUMERIC-POLICY-ID>RAW ( CAD-KIND:numeric-policy-id -- n ) ;

: ID-CK ( CAD-KIND:numeric-policy-id -- n )
   NUMERIC-POLICY-ID>RAW dup 0 < over DOM-COUNT >= or if E-NPOL-DOM throw then ;

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

: R-OK ( a -- id-result<a> )          NPOL-ID--RESULT:OK ;
: R-WRONG-WIDTH ( -- id-result<a> )   NPOL-ID--RESULT:WRONG-WIDTH ;
: R-UNKNOWN ( -- id-result<a> )       NPOL-ID--RESULT:UNKNOWN ;

public

\ REGISTER interns a numeric policy: the content is its single proof dom, so equal
\ doms intern to one id (content-addressed). The ONLY authority-bearing public mint,
\ bound to a real dom value, never a raw cast.
: REGISTER ( dom -- CAD-KIND:numeric-policy-id )
   DOM>N RAW>NUMERIC-POLICY-ID ;

\ POLICY-DOM projects an id back to its proof domain (fail-closed on a corrupt id).
: POLICY-DOM ( CAD-KIND:numeric-policy-id -- dom )
   ID-CK N>DOM ;

: VALIDATE ( CAD-KIND:numeric-policy-id -- CAD-KIND:numeric-policy-id )
   dup ID-CK drop ;

\ ID>WIRE is total for a valid id (E-NPOL-WIRE if the cap cannot hold the fixed
\ width); WIRE>ID reads the fixed width, validates the raw FAIL-CLOSED, and refines
\ to the nominal id only on success (§ 23.9).
: ID>WIRE ( CAD-KIND:numeric-policy-id ptr u8 n -- n )
   {: id:CAD-KIND:numeric-policy-id out:ptr cap:n :}
   cap WIRE-BYTES < if E-NPOL-WIRE throw then
   id ID-CK  out  WIRE-BYTES  LE-PUT
   WIRE-BYTES ;

: WIRE>ID ( ptr u8 n -- id-result<CAD-KIND:numeric-policy-id> )
   {: a:ptr u:n :}
   u WIRE-BYTES <> if R-WRONG-WIDTH exit then
   a WIRE-BYTES LE-GET {: raw:n :}
   raw 0 < raw DOM-COUNT >= or if R-UNKNOWN exit then
   raw RAW>NUMERIC-POLICY-ID R-OK ;

\ CROSS-PROCESS CONTENT KEY (KEY>WIRE / WIRE>KEY). Numeric-policy is the documented
\ EXCEPTION to the § 23.9 origin-class 32-byte SHA-256 form: a numeric policy's ENTIRE
\ content is its single proof `dom`, whose canonical serialization is the rank ordinal
\ 0..3 (DOM>N / N>DOM), and the § 23.9 numeric-policy-id row explicitly blesses that
\ minimal wire precedent. The rank is the enum ORDINAL, not a process-local registry
\ INDEX, so it is ALREADY cross-process-stable (rank 2 is `relative` in every process)
\ and content-addressed - wrapping it in a 32-byte SHA-256 would be pure ceremony over a
\ 2-bit value and would not improve cross-process identity. So this family's canonical
\ content key IS its 8-byte rank, and the content-key codec coincides with ID>WIRE /
\ WIRE>ID. It is exposed under the KEY>WIRE / WIRE>KEY names so the envelope calls a
\ uniform cross-process surface across every content-addressed family.
: KEY>WIRE ( CAD-KIND:numeric-policy-id ptr u8 n -- n )   ID>WIRE ;
: WIRE>KEY ( ptr u8 n -- id-result<CAD-KIND:numeric-policy-id> )   WIRE>ID ;

;package
