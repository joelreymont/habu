---
title: Model construct row surgery
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:04:00.458619+02:00"
---

Full context: one of the omission leaves split out of habu-model-the-declared-4a2eb3c9. That parent dot named "construct (CONM) and field projection" as a single omission; reading the code shows they are two different rules with two different owners, so they are split. This leaf is construct. Field projection is habu-model-field-projection.

What is being discharged. formal/Common/Effects.v lists "construct (CONM) and field projection, which are row surgery over the same layout machinery" and formal/Common/Control.v lists "construct (CONM), the field projection window, and the transport ops". This leaf removes the construct clause from both headers.

What the checker actually decides today with no model behind it. construct is a three-token form and a small state machine, not a word call. CONSTRUCT-BEGIN (src/core/checker.f:9101) arms CONM to 1; CONSTRUCT-TOK (checker.f:9109) consumes the family token, resolves it through CONSTRUCT-FAM-XT and moves to 2, then consumes the variant token, resolves it through CONSTRUCT-STEP-XT and applies the generated-constructor effect inline before clearing CONM. DO-TOK1 checks CONM before the match interception and before the ordinary control dispatch (checker.f:9172), so while the form is open EVERY token is captured whatever it spells - which is what stops a failed resolve turning into an uncheckable undefined word. CHECK latches MD-CON-TRUNC and fails the definition when CONM is still non-zero at the boundary (checker.f:9762, 9784). The step effect itself lives in TFC-CONSTRUCT-STEP-VID (src/core/type-family.f:2428): the variant payload is consumed and the family bundle is produced through PUSH-LOGICAL, which Effects.v already models.

Where the rule belongs. formal/Common/Control.v, next to the MATCH section, because construct is a token machine exactly like MATCH: a construct sub-mode field on the state, a token for the construct spelling, the capture rule, the truncation latch at the boundary, and the step effect written in terms of Effects.v's push_logical. The results worth stating: the form always captures three tokens; an unterminated form is refused at the boundary; and construct followed by a match on the same family and variant returns the payload it was given, which is the round trip that makes the two rules each other's inverse.

The vector shape that would bind it. Shared program vectors in test/compiler/checker-model-schema.f. Note the scope rule: TFAM-CONSTRUCT-FAM resolves the family in the ACTIVE package only, so a construct vector only certifies when the checker is asked from inside the package that owns the family. The gate must therefore run inside that package.

The mutation that must go red. Remove the CONM clause from CHECK's open-form test (checker.f:9784) so an unterminated construct is no longer refused, and remove the step application from CONSTRUCT-TOK (checker.f:9115). Each must turn exactly one row red. Restore src/core/checker.f byte-identically and record the matrix.

Blocked by nothing. It shares Effects.v's layout machinery with habu-model-whole-bundle-9589e059, so do not run the two in one workspace.

Implemented by agent=modeldecl in workspace .jj-ws/habu-model-the-declared-4a2eb3c9,
under the parent dot habu-model-the-declared-4a2eb3c9's active claim, in the
commit that follows the split. Not yet reviewed or merged, so this dot stays
open until the orchestrator lands it.

MEASURED.

What landed. formal/Common/Control.v gained a construct sub-mode: one state
field carrying both CONM and CONFAM (so a resolved family while the family token
is still awaited cannot be written down at all), a TConstruct token, the capture
rule ahead of the match interception exactly as DO-TOK1 orders it, the step
written as the variant payload in and Effects.v's push_logical of the family
term out, and the truncation latch and open-form refusal at the definition
boundary. Five published results, all with proofs and no assumptions: the step
consumes the payload and produces the bundle, an unterminated form is refused
with MD-CON-TRUNC, an operand is captured whatever it spells, the payload
belongs to the variant and not the family, and construct and MATCH are each
other's inverse. Eight shared vectors CMV19-CMV26 in
test/compiler/checker-model-schema.f, so every one of those results is asked of
the shipped checker as well as of Rocq. The construct clause is gone from both
model headers.

What the gate needed to be able to ask the question. construct resolves its
family in the active package only, so the same text certifies inside the
declaring package and is refused outside it. test/compiler/checker-model-proof.f
now runs the whole gate inside package CHECKER-MODEL-CASES. The eighteen
pre-existing rows answer identically either way, measured green before and after
the move.

Falsification matrix. Each mutation was applied to the shipped source, the
fixpoint was rebuilt with bin/hb --load tools/build-fixpoint-refresh.f --
install --force, the gate was rerun, and the source was restored byte-for-byte
(verified by checksum and by the final jj diff, which does not touch
src/core/checker.f at all).

- Drop the CONM clause from CHECK's open-form test, src/core/checker.f:9784.
  Exactly one row red: an_unterminated_construct_is_refused_at_the_boundary,
  expected VReject, got VCert. Nothing else moved.
- Delete the inline step application from CONSTRUCT-TOK, checker.f:9115. Four
  rows red: construct_builds_the_bundle_from_the_variant_payload,
  a_construct_operand_is_captured_whatever_it_spells,
  the_payload_is_the_variants_and_not_the_familys, and
  construct_then_match_returns_the_payload_it_was_given.
- Make the family operand close the form instead of opening the variant slot
  (2 CONM ! becomes 0 CONM ! at checker.f:9111), which is the capture removed.
  Seven rows red - every construct row except the uncheckable control, which is
  the row with no construct in it.
- Two attempts to falsify the payload rule inside src/core/type-family.f were
  not usable and are recorded as findings rather than as matrix rows. Making
  TFC-PAY-ROW consume no payload stops lib/ itself certifying: the fixpoint
  self-check refuses the build at split-next, so the payload row is load-bearing
  for the engine and the gate never runs. Making the reserved-token step use the
  next variant id throws "tfam: bad variant id" at run time. The payload rows
  are still falsified, by the two checker.f mutations above.

Honest gaps left behind.

- Package ownership is not represented. TFAM-CONSTRUCT-FAM's active-package rule
  lives in src/core/type-family.f, and these models are models of
  src/core/checker.f, so an operand that named nothing, one that named a
  non-sum family, and one that named a family owned elsewhere are one case in
  the model. That is faithful downstream - all three clear OK, latch a reason
  and let the form consume its second operand - and it is now written into
  Control.v's own omission list. MATCH's family token already had the same
  abstraction.
- The model's construct is arity-0. TFC-CONSTRUCT-STEP-VID also recovers
  concrete family arguments from the declared output for a multi-cell
  instantiation (CONSTRUCT-DECL-MULTICELL?) and stages an open-argument
  instantiation fail-closed. Neither is modelled. A generic family's construct
  is therefore outside the modelled fragment; the arity-0 case is what the
  vectors reach.
- MD-CON-KIND (a construct family that is not a sum or an enum) has no vector.
  It collapses into the same unresolved-operand case as an unknown name.
- The reason codes MD-CON-TRUNC and MD-CON-VAR are pinned by check_reason on
  the Rocq side only; the gate compares verdicts, not reasons. Both were
  measured against the shipped checker's rendered diagnostics by hand
  ("bad construct: missing family or variant token" and
  "bad construct: unknown variant") but nothing holds them there automatically.

Is this the best long-term solution or a patch? Long-term. The construct rule is
modelled where it belongs - as a token machine beside MATCH, since that is what
src/core/checker.f makes it - and its step reuses Effects.v's existing
push_logical rather than a private copy, which is why construct and MATCH come
out as each other's inverse instead of two unrelated rules that happen to agree.
The state is one field with three shapes rather than two independent numbers, so
the checker's own "CONFAM is only meaningful while CONM is 2" invariant is
structural in the model instead of a comment. Nothing rests on a magic number:
the reason codes are named constants read off the checker's own table, and every
vector is a two-sided obligation with one verdict written once. The gate's move
into the declaring package is a structural consequence of the owner-only
resolution rule, not a workaround for it: there is no other place from which the
question can be asked at all.
