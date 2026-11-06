---
title: Reuse one CP-OTHER in FIELD-PROOF
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T03:59:53.831323+02:00"
---

Problem: FIELD-PROOF (src/core/checker.f) mints a fresh stale CP-OTHER per call (stuart, hbind fix, 2026-11-07); each call grows the checker's tables. Verify first: count CP-OTHER mints over one load of test/wide-typed-local-probe.f. Acceptance: one shared value or a reason in FIELD-PROOF's comment why each call needs its own; probes unchanged. Files: src/core/checker.f. Verify: test/wide-typed-local-probe.f, test/catch-stale-suite.f. Depends: none. Ownership: carl. Claim: unassigned.
