---
title: Latch value-position refusals through one diagnostic path
status: active
priority: 3
issue-type: task
created-at: "2026-09-17T18:44:23.890544+03:00"
---

Problem: src/core/checker.f (rule commit 1204d189, habu-refuse-a-ptr-5ad2734e) latches the raw-cell refusals through RAW-DIAG-TOK! and RAW-PTR-DIAG-CLASSIFY, a second path beside MDIAG!, because CHECKER-STEP sets FAILSET the instant UNIFY-IN fails and MDIAG! only latches while the pin is open, so the MD-EXEC-OPAQUE precedent cannot name a value-position refusal. Acceptance: CHECKER-STEP gains a reason hook (the failing token's reason recorded before FAILSET closes the pin) so MDIAG! latches value-position refusals too, the two raw-cell latch words collapse onto it, and the raw-cell fixtures (test/compiler/raw-cell-pointer-refusals.f) keep their diagnostics verbatim. Files: src/core/checker.f, test/compiler/raw-cell-pointer-refusals.f. Verify: the fixtures; test/checker suites; fixpoint; test/run.f. Depends: habu-refuse-a-ptr-5ad2734e landing. Ownership: checker diagnostics. Claim: agent=hazel-diag-latch workspace=.jj-ws/hazel-diag-latch.
