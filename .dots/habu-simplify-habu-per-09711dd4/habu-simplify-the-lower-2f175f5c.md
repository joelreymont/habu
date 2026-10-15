---
title: Simplify the lower-cert install chain
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.941361+03:00"
---

Problem: the lower-cert producer is a two-level single-assignment chain (checker.f:11557-11575; lower-cert-base.f:57-63, 119-132; layout-valid.f:392-396; lower-cert-seal.f:12-28, which undefines nine names), and it has only one installer. lower-cert-base.f:136-157 wraps private constants in 22 colon words. About -45 lines. Acceptance: FULL-XT defaults to the EMPTY producer; the install flags, guards and seal are deleted; the constant wrappers are deleted; lower-cert behavior unchanged. Files: src/core/checker.f, src/core/lower-cert-base.f, src/core/layout-valid.f, src/core/lower-cert-seal.f. Verify: rebuild bin/hb (checker is in the engine prefix); checker suites named by test/gate-stdlib-cases.f; bin/hb --load test/run.f (shared checker behavior). Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. Ownership: listed files. Claim: unassigned.
