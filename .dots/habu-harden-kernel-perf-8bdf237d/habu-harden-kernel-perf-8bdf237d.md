---
title: "Harden kernel-perf-lint: hunk-aware diff, waiver ratchet, IR watch-set"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T18:16:23.861730+02:00"
blocks:
  - habu-lint-diff-share-486c2d86
  - habu-kernel-perf-ratchet-7d99cab2
  - habu-kernel-perf-complete-437a1b83
---

Problem: kernel performance promotion can be bypassed through three independent gaps: kernel-perf parses header-looking added lines outside hunks, WAIVER rows are not bound to emitter identity or deterministic expiry, and PTX IR/tile/optimizer producers are absent from the watch set. Typed-local already has local hunk state, so duplicating it is forbidden. Fix: coordinate three subdots in order: shared checked unified-diff events for both linters, deterministic next-touch waiver ratcheting with canonical emitter ownership, then the complete exact IR producer watch set. Acceptance: each child is reviewed and green; spoofed headers, cross-emitter/stale waivers, and every omitted producer fail closed; no wall-clock policy or broad path prefix is used. Ownership: coordination only; child dots own exact files and fixtures.
