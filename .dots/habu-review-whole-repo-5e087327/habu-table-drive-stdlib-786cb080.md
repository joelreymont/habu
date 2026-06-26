---
title: Table-drive stdlib manifest policy
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-25T12:19:43.555550+02:00\""
closed-at: "2026-06-25T21:21:57.025493+02:00"
close-reason: Replaced hard-coded stdlib manifest doc policy and module-note branch ladder with checked row groups plus one module-note row validator in tools/stdlib-manifest-test.f. Manifest format unchanged. Validated focused stdlib-manifest-test and full native gate.
---

Finding F15. Evidence: docs/factorization-review.md:43; tools/stdlib-manifest-test.f:329 and tools/stdlib-manifest-test.f:390. Root cause: manifest doc policy is a hard-coded checklist and branch ladder. Fix: represent required docs and module note rules as checked rows and drive one row validator. Why: policy data should be inspectable and extensible without branch growth. Validate with stdlib-manifest tests and full native gate.
