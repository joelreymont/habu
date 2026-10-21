---
title: Move capture self-tests out of aot-capture.f
status: closed
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.060506+03:00"
closed-at: "2026-10-21T20:04:02.534613+03:00"
close-reason: "landed in 3bdffeeafa17 (lane cf91b6118389): aot-capture.f load-time pool/WID self-tests and per-capture index/record re-proofs removed, external input checks (stale index, pool/dictionary bounds, ACAP-COMPACT-ONE field refusals, band and wid audits) kept; independent review 2026-10-21 ACCEPT after the stale span-proof claim in src/habu/aot-decl.f was dropped; master full native suite and two-generation build pass"
---

Problem: loading aot-capture.f runs synthetic pool and WID self-tests, and each capture rechecks its own target index, name index, and compact record expansion. Remove those internal proofs and unused helpers while preserving capture output and external artifact validation. Existing capture, boot, out-of-line-name, and WID rebase tests cover the observable behavior, so duplicating the synthetic assertions in a new gate case would add no distinct coverage. Claim: mark-capture-cleanup from 2e2a1f98, authorized to work before Carl's combined lane lands. Capture source cleanup and focused capture checks are complete; keep active pending integration, combined native/convergence qualification, and independent review owned by the lead.
