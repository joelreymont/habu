---
title: "Checker capability: layout-polymorphic family params"
status: active
priority: 2
issue-type: task
created-at: "\"\\\"\\\\\\\"2026-07-03T23:36:48.964243+02:00\\\\\\\"\\\"\""
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
