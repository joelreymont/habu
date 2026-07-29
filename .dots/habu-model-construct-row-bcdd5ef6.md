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
