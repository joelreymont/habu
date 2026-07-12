---
title: "Seal owner packages: syntax and sinks"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T15:41:55.221170+02:00"
blocks:
  - habu-seal-owner-pkgs-6020cc5b
---

Implement phase 3 of habu-checker-seal-owner-f7de26ff after checker registry. Add lowercase `sealed-package NAME ... ;sealed-package` grammar sharing normal package open/close internals; atomically commit public/private WID pair only after checker preflight; reject mismatched closers and leaked authority. Enforce resolved-WID guards at reopen, qualified publishers, EXPORT source/target, undefine, tick/bracket-tick/postpone, set-current publication, immediate, hide/forget truncation. Extend verify-source, public-signature, namespace/reserved/source-discovery/all-errors/typed-diff scanners and bootstrap compiler parity. Acceptance: test/seal-owner-package.f covers --load/stdin/bootstrap/AOT/snapshot hostile matrix rc84; ordinary reopen and sealed public calls green. Full fixpoint/bootstrap/AOT/test/lint gates.
