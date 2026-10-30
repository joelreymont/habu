---
title: Make the two ORDERED definition inputs re-checkable
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T08:33:47.451809+02:00"
---

Problem: two definitions record an ORDERED dependency row (tools/native-unit-dependency.f ORDERED: a fact no later build can re-verify), so under the fasl load model they compile live in every build instead of being reused: DOES-RECORD in src/compiler/native/publish.f (does-record primitive state unknown) and KEEP? in src/core/checker-surface.f (the fixed value IMAGE-WHITEBOX unknown). Found by fasl lane 7 (habu-spike-the-fasl-16b76e28). Acceptance: each input recorded as a row NUNIT-DEP:CHECK can judge (the does-record state and the whitebox value observed through OBSERVE-BOUND or a typed row of their own), the two definitions reuse on a warm build (no live-head diag line), and a change to either input misses the head. Files: tools/native-unit-dependency.f (the ORDERED reasons, OBSERVE-BOUND), src/compiler/native/publish.f, src/core/checker-surface.f. Verify: a warm build with diag on shows no live-head line; the fasl proof runs. Depends: habu-spike-the-fasl-16b76e28. Ownership: the two definitions' inputs and their rows. Claim: unassigned.
