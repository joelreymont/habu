---
title: "Seal owners: syntax and sinks"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T16:07:29.161830+02:00"
blocks:
  - habu-seal-owners-checker-d750c106
---

Problem: no first-class language grammar can atomically close an ordinary owner and enforce its public/private WIDs at every mutation sink. Acceptance: add lowercase sealed-package NAME and ;sealed-package sharing package internals; atomic checker/native commit; mismatched closers and leaked authority reject; resolved-WID guards cover reopen, qualified publishers, EXPORT, undefine, tick, bracket-tick, postpone, set-current publication, immediate, hide and forget. Extend verify-source, public-signature, namespace, reserved, source-discovery, all-errors and typed-diff scanners plus bootstrap parity. Files: src/habu/habu2.f, src/habu/xref.f, src/habu/verify-source.f, bootstrap/cg/forth.fs, tools scanner owners, test/seal-owner-package.f, FILEMAP.md, docs/forth.md. Verify: load/stdin/bootstrap/AOT/snapshot hostile matrix rc84, full fixpoint/bootstrap/AOT/test/lint gates. Depends: habu-seal-owners-checker-d750c106.
