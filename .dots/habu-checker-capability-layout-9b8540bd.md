---
title: "Checker capability: layout-polymorphic family params"
status: active
priority: 2
issue-type: task
created-at: "\"\\\"\\\\\\\"\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"2026-07-03T23:36:48.964243+02:00\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\"\\\\\\\"\\\"\""
---

Follow-on capability, not in PLAN.md v1: item 12 rejects layout types in cell-only family parameters, so span<result<...>>/containers of ADTs are inexpressible. Add layout-polymorphic parameter kinds (families declaring param kinds that accept layout values with width propagation into instantiation and lowering) so collections of sums/products type-check. Needs: param-kind metadata (TFAM 2a records), width-aware instantiation (TFAM 12 machinery), negative fixtures flip to positive. Depends: TFAM 12, 16.

Claim: agent=laycap workspace=.jj-ws/fable-laycap (checker lane - owns src/core/checker.f + type-family.f + fixtures this session)

DESIGN OF RECORD 2026-07-17 (laycap lane, survey-only - NO code landed,
correctly: no sound gated unit fits one session; claim RELEASED).
Before-state: probes P1/P2/P3 (option<off len>, option<ptr u8 n>,
result<n n,n>) reject at SIG-TYPE's one-type-per-slot separator check
(checker.f:2582-2593) with SIG-END-PARAM conflating param count with
cell count; fail-closed exit 70 proven. CRUX (empirical): even a
multi-cell PRODUCT as a param parses but rejects at unification -
OPTION:SOME's fresh var is single-cell and cannot absorb a hidden-field
value - so a multi-cell param must be ONE term (tagless flat tuple of
width W) that a generic var binds whole, expanded via PUSH-LOGICAL;
argc==TFAM-ARITY preserved. Surfaces: PK-LAYOUT param-kind (constant
reserved, TF-PK pool exists, nothing consumes it yet); T-WIDTH must
become arg-aware (11 TFAM-WIDTH@* call sites route through it);
TFC-PAY-ROW/TFC-SCH-TERM PUSH-LOGICAL expansion; lowering pads
(TFL-VPADS) are declared-metadata-keyed and WRONG for parametric widths
- construct/match lowering must specialize per call-site width via the
17 WF-fact + EM-P2-TRIGGER mechanism in BOTH native and gforth emitters.
Pins inventory + gate wiring recorded in the lane report; no existing
negative pins the literal probe shapes (positives get ADDED; adversarial
negatives: wrong-width, cross-family, linear payload stay red).
SLICES (each independently gatable): (1) tuple term + arg-aware T-WIDTH
+ route 11 width sites (no new accepts); (2) PK-LAYOUT parser groups +
declaration marking (sig-parse accepts, construct fail-closed); (3)
constructor/MATCH effects (probes CERTIFY, lowering staged fail-closed
+ named boundary); (4) width-aware native+gforth construct/match
lowering (runtime round-trips, staged reject flips, full byte-fixpoint
battery) - slices 1-4 = the flat wave-B unblock; (5) nested ADTs +
linear payloads stage-lift.

Claim: agent=laycap1 workspace=.jj-ws/fable-laycap1 (SLICE 1 only: tuple term + arg-aware T-WIDTH + width-site routing; no new accepts)

SLICE 1 LANDED 2026-07-18 (laycap1 lane, commit ff33f023 + integration
fixes; claim RELEASED): T-WIDTH is arg-aware (TFAM-INST-WIDTH@ walks
variant/product schemas substituting each param's instantiated arg
width via a forward hook), all 11 TFAM-WIDTH@* checker call sites
routed, the term representation confirmed as the EXISTING resolved
fam<args> T-PARAM term (no parser coupling - the design of record
holds). Width-correctness proven: opt<pt3> computes 4 where the
family-only width was a degenerate 2; cell-width args stay behavior-
preserving. NO new accepts: the three probe shapes now PINNED rejecting
(tdpbopt/tdpbres fixtures). Fixpoint x2 646a9979 (changed vs base as
expected - baked source; x2-stable is the invariant; re-verified
identical on the integration tree); the slice's HELD run.f verdict was
caused by the pre-existing refine-lint red from the maction landing
(RAW>ACTION-ID unseeded) - fixed on master (6cfaacc2) before this
integration; the 3 new test wrappers' classification rows added at
integration (the test/-is-exempt assumption was wrong - inventory
counts test-metaprog sites). NEXT: slice 2 (PK-LAYOUT parser groups).

Claim: agent=laycap2 workspace=.jj-ws/fable-laycap2 (SLICE 2 only: PK-LAYOUT parser param groups + declaration marking; sig-parse accepts, construct stays fail-closed)

DOR AMENDMENT 2026-07-18 (laycap2 stop + orchestrator decision): SLICE 2
AS WRITTEN WAS NON-BOUNDED - two couplings proven: (A) the raw-run tuple
term was never built (slice 1's "existing fam<args> term suffices"
conclusion is valid for NAMED layout-family args only; an anonymous run
like option<off len> has no width-W representation, and building one
means a reserved registry family + sealing + rendering + re-opening the
landed slice-1 decision); (B) the declaration surface (LAYOUT header
clause) lives in src/core/sumtype.f per the 18 one-grammar rule -
outside the slice write set. DECISION: re-scope the program to NAMED
payload products - option<str-slice> with PRODUCT str-slice ptr u8 n is
the better Habu shape (small typed domain words over raw tuples), the
lane's probes prove named args ALREADY parse-accept with construction
fail-closed, and wave-B migrates by minting small named payload products.
NEW SLICE MAP: slice 2' = pin named-arg acceptance + fail-closed
construction (fixtures/docs only, landing now); slice 3 = constructor/
MATCH effects for multi-cell NAMED args (TFC-SCH-TERM/TFC-PAY-ROW
PUSH-LOGICAL expansion - the probes' verdict-0 constructions flip);
slice 4 = width-aware dual-emitter lowering (unchanged); slice 5 =
nested/linear stage-lift (unchanged); slice 6 (OPTIONAL sugar, staged) =
raw-run fold: reserved tuple family + TFAM-INST-WIDTH@ tuple branch +
SIG-TYPE PK run-fold + TFAM-PK read-hook + sumtype.f LAYOUT clause -
Couplings A/B above are its spec; requires expanding the write set to
sumtype.f when claimed. Wave-B's migration note updated accordingly.

SLICE 2' LANDED 2026-07-18 (laycap2 lane, commit 2f5b42c7; claim
RELEASED): named-layout-arg acceptance pinned positive (TDPN1-3 incl.
two-layout-arg result), fail-closed constructions pinned with exact
diagnostics (TDPN4-7; TDPN5 pins the ARG-AWARE W=3 tag slot - a live
validation of slice 1), docs 18 corrected (its nested-reject claim was
empirically false - nested named applications identity-accept today,
construction fail-closed; noted, unpinned, slice-5 territory). Raw runs
stay pinned rejecting (TDPB1-3). NEXT: slice 3 - constructor/MATCH
effects for multi-cell NAMED args (TFC-SCH-TERM/TFC-PAY-ROW
PUSH-LOGICAL expansion; the TDPN4-7 verdict-0 constructions flip to
positive with adversarial negatives preserved; MATCH payload binding).

Claim: agent=laycap3 workspace=.jj-ws/fable-laycap3 (SLICE 3: constructor/MATCH effects for named multi-cell args; lowering stays fail-closed staged)

SLICE 3 LANDED 2026-07-18 (laycap3 lane, commit 67d18669 after rebase;
claim RELEASED): the capability FLIP. TFC-PUSH-PAY expands payload terms
only at T-WIDTH>1 (W=1/cells/open vars untouched - the maki-preserving
gate); TFC-CONSTRUCT-STEP-VID recovers concrete args from the declared
output; the generated-constructor CALL is intercepted in DO-TOK and
routed arg-aware only when the declared output binds a genuinely
multi-cell arg. TDPN4-7 FLIPPED (SOME/NONE/OK/ERR on named W>1 args
certify); MATCH destructures the exact hidden cells (ML1); four
adversarials red (wrong-width x2 incl. the W=3 slot diag, cross-family,
linear); raw-run/identity/ZP/MB19/linear pins all preserved. LOWERING
STAGED FAIL-CLOSED: a real definition constructing a parametric
multi-cell instantiation dies rc-70 E-REJECTED (CLFC1-3 pinned) - never
a silent miscompile. Fixpoint x2 8b2f8d72 (re-verified identical on the
integration tree); the slice's two battery reds were proven artifacts
(STATUS date - already fixed on master; cad replay parent!=child engine
- clears with the installed binary; both confirmed green at integration).
NEXT: slice 4 - width-aware construct/match lowering in BOTH emitters
(native EM-ADT-CON-PUSHES + gforth mirror CMPADS-CELL) via the 17
WF-fact + EM-P2-TRIGGER mechanism; the CLFC staged rejects flip; runtime
round-trips land; then option/result get marked and wave-B unblocks.

Claim: agent=laycap4 workspace=.jj-ws/fable-laycap4 (SLICE 4: width-aware construct/match lowering in BOTH emitters; CLFC rejects flip; runtime round-trips)

SLICE 4 LANDED 2026-07-18 (laycap4 lane, commit 13f8a504 after rebase;
claim RELEASED): the FLAT CAPABILITY IS COMPLETE (slices 1-4). Width-
aware construct/MATCH lowering via the extra-pad WF-XPAD call-site fact
(recorded only when extra>0 - no fact, no pass-2, existing shapes
byte-identical by construction AND empirically cmp-proven); pass-2 adds
the delta zeros in all three legs (reserved construct, MATCH OF,
generated-ctor CALL) in BOTH emitters - DDC gforth chain == native
fixpoint 36bf9828 byte-identical (the DoR's flagged dual-emitter risk
verified). CLFC1/2 flipped to real compiled runtime round-trips;
CLFC-NESTED preserves the slice-5 boundary; option/result needed NO
declaration edit (they already accept named args - the real families
round-trip SOME/NONE/OK/ERR over a named product); the three wave-B
probe shapes pinned END-TO-END via named payload products. The WF-flag
ripple spanned five coordinated sites (cert constant, PPRIM, VALIDATE-WF
mask, PEINV manifest, AX-CENSUS) - the malformed-certificate symptom is
the tell. Integration: engine refreshed to 36bf9828, ctor suite + maki
+ DDC + FULL run.f green with perf-verdict pass attempts=1 (the two
worker-side reds were the proven stale-binary artifacts). REMAINING on
this dot: slice 5 (nested ADTs + linear payloads stage-lift) and slice 6
(optional raw-run sugar, Couplings A/B spec).

Claim: agent=laycap5 workspace=.jj-ws/fable-laycap5 (SLICE 5: nested ADTs + linear payloads stage-lift; CLFC-NESTED flips; write set: checker + both emitters + fixtures, sumtype.f if the nested header needs it)

SLICE 5 LANDED 2026-07-18 (laycap5 lane, uncommitted-then-committed in this
workspace; NOT pushed, dot stays active). The flat capability's two open items
close with a SMALLER surface than the write set anticipated: NO emitter edit, NO
new WF fact kind, NO 5-site ripple, NO sumtype.f change.

MECHANISMS.
(1) NESTED named ADTs (option<result<n,pkg:prod>>) construct/certify/lower/
MATCH end-to-end. Discovery: the checker ALREADY certified nested candidates
before this slice — every width site recurses (TFAM-INST-WIDTH@ via the
SCHEMA-PARAM->PARAM>ARG->T-WIDTH path, TFC-VAR-PAYCELLS, famterm T-WIDTH), and
PUSH-LOGICAL expands to the arg-aware hidden-field count. The ONLY blocker was
the flat-only gate TFC-CON-FLAT? (rejected any arity>0 arg) driving
CONSTRUCT-WIDE-STAGED-REJECT on real compiles + suppressing the extra-pad fact.
Replaced it with the recursive width-stability check TFC-CON-CLOSED? (no open
type var anywhere in the arg tree; reentrant locals+RECURSE). The SAME extra-pad
WF-XPAD model lowers each level — the inner bundle carries its own pads at its
own construct site, the outer construct/match adds only the outer delta — so the
existing pass-2 legs in BOTH emitters and the existing WF-XPAD fact suffice
UNCHANGED. Verified end-to-end runtime round-trips at depth 2 and 3 (product
leaf): CN-RT-SOMEOK=5, CN-RT-SOMEERR=17, CN-RT-NONE=999, CND-RT-SOME=7.
(2) LINEAR through nesting: FIXED A REAL SOUNDNESS HOLE. option<lq2<ltok,n>>
read as NON-linear (the direct arg lq2<ltok,n> is a T-PARAM, not a con), so it
could dup/drop-launder the buried ltok (NL-DUP was ACCEPTED before slice 5).
Made linearity transitive: LAYOUT-ARG-LINEARISH? and LAYOUT-ARG-LIN-N recurse
through a nested layout arg (self-recursive RECURSE), LIN-TYPE-COUNT still
samples once at the bundle tag (no double count). Sound rule now holds: construct
consumes once, MATCH re-introduces once, dup/drop/none-loss/match-then-drop
reject. Also corrected the slice-3 TDPA4 FALSE-NEGATIVE (`( own -- tdpbopt<own> )`
rejected on UNDECLARED `own`, not a linear violation — linear multi-cell
construction has been sound since item 11).

FLIPPED PINS: CLFC-NESTED (type-ctor-suite, rc70 staged reject -> real
round-trip). TDPA4 (type-decl-suite, false-negative -> real tdown con, positive
construct + dup reject TDPA4B).
ADDED PINS: type-ctor CN-RT-SOMEOK/SOMEERR/NONE/NONER, CND-RT-SOME/NONE, plus
real-compile fail-closed CN-BADW/BADF/BADS/TRUNC/OPEN (rc70); type-decl
TDNEST1-4, TDNOPEN (candidate certifies), TDNA1-3 (E-MISMATCH + arg-aware inner
slot @tdnres.slot2 diagnostics); type-match MN1-3 (nested MATCH destructure/
imbalance/re-wrap); type-linear NL1-7 (nested-linear construct/dup/drop/none-loss/
identity/match-remint/match-drop).

SOUND BOUNDARY (not a gap): an OPEN inner var (option<result<n,a>>) has an
UNSTABLE width, so it certifies as a candidate but its REAL compile stays
fail-closed via CONSTRUCT-WIDE-STAGED-REJECT (CN-OPEN, rc70). Declaration-level
nested application over a family's own param (VARIANT wrap dinner<a>) does NOT
parse today, so the SCH-NODE-IWIDTH SCHEMA-APP branch only ever sees arity-0
concrete apps (declared width == instantiated) — no latent width gap, nothing to
5b. NO 5b remainder: the sound subset for nested + linear-through-ADT is complete.

RCA (blocking, resolved): the first draft used `defer LAYOUT-MAYBE-LIN-XT` /
`defer LAYOUT-LIN-COUNT-XT` forward refs for the mutual recursion. These were the
FIRST pre-trust defers in src/core/checker.f (the tree declares ZERO;
checker.f:7947), so the DRAIN-PRETRUST-COMPAT miss branch no longer tolerated an
empty table -> gate-stdlib pre-trust-defer "old-engine tolerance" hit the
SEAL-CAPTURE backstop, exit 73. Root-caused via the C-DEFER->C-PD-CAPTURE path
and re-fixed by making the recursion SELF-recursive (RECURSE), removing both
defers. (TFCL-NODE-XT is not a counterexample: it is in type-family.f, loaded
after `: TRUST`.) gate-stdlib green after the fix.

BYTE-IDENTITY (slice-4 discipline): emitter files UNTOUCHED; TFC-CON-CLOSED? ==
TFC-CON-FLAT? on every non-nested input; the linear recursion only branches on a
T-PARAM arg. Proven by jitdump cmp of a non-nested WIDE program (NW=clopt<clw2>
none extra-pad, NS=clopt<clw2> some) between parent-src and slice-5-src on the
same seed binary (bin/hb loads src/core at boot): BOTH byte-identical.

GATE TABLE (all green, YOUR workspace, fixpoint a3761eae after RCA fix):
  fixpoint x2 byte-identical  = a3761eae5eb068a18df24aa3453040df5c10cc6d562f0272f81bcaf830a76aee (x2 cmp identical)
  old-binary boot (b4d951cb)  = rc 0
  DDC dual-emitter            = ddc: byte-identical a3761eae... (gforth chain == native fixpoint)
  run.f                       = perf-verdict performance=pass correctness=t attempts=1, no FAIL
  type-ctor/decl/match/linear = ok ; type-family-suite = ok
  maki/test.f                 = ok (device leg skipped off-device)
  test/gate-stdlib.f          = 0 red phases (pre-trust-defer PASS)
  host-lint                   = 0 finding(s)
  filemap-lint                = 0 finding(s) (917 paths)
  trusted-inventory -- strict = baseline (separable folds 2/2; no new trusted surface)
  typed-local-diff-lint       = rc 0
  (run.f internal lint battery: parity/shadow/clobber/trust/status/filemap all PASS)
Files: src/core/checker.f, src/core/type-family.f, docs/type-families.md (18/19),
LESSONS.md, test/type-{ctor,decl,match,linear}-suite.f.

SLICE 6 (still open, OPTIONAL sugar): raw-run fold (option<off len>, result<n n,n>)
= reserved anonymous tuple family + TFAM-INST-WIDTH@ tuple branch + SIG-TYPE PK
run-fold + TFAM-PK read-hook + sumtype.f LAYOUT header clause (Couplings A/B in
the 2026-07-18 DoR amendment); expands the write set to src/core/sumtype.f. The
preferred Habu shape remains a small named payload product per domain pair
(needs no new grammar), so slice 6 is sugar, not a blocker.
