---
title: Move capture self-tests out of aot-capture.f
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.060506+03:00"
---

Problem: loading aot-capture.f runs synthetic pool and WID self-tests, and each capture rechecks its own target index, name index, and compact record expansion. Remove those internal proofs and unused helpers while preserving capture output and external artifact validation. Existing capture, boot, out-of-line-name, and WID rebase tests cover the observable behavior, so duplicating the synthetic assertions in a new gate case would add no distinct coverage. Claim: mark-capture-cleanup from 2e2a1f98, authorized to work before Carl's combined lane lands. Capture source cleanup and focused capture checks are complete; keep active pending integration, combined native/convergence qualification, and independent review owned by the lead.
