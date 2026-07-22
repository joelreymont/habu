---
title: "Source origins: intern declaration spans"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T12:22:51.936123+02:00"
blocks:
  - habu-src-frames-restore-869b0245
---

Problem: declaration metadata cannot retain transient parser pointers or allocation-order frame ids. Acceptance: add one immutable package-owned origin arena that captures a live authenticated frame, exact byte start and length, and bounded canonical parent-origin chain. The canonical key is logical source identity, required content digest, byte span, and parent chain; reject stale frames, out-of-range spans, cycles, depth overflow, and identity mismatch before publication. The raw arena index remains internal and no legacy type declaration or second frame stack is added. Files: narrow source-origin arena, reflection for canonical identity fields, and focused direct/nested fixtures. Verify: duplicate interning, independent frames, invalid spans, cycles, maximum depth, and allocation-order independence. Depends: Source frames: restore nested parents. Ownership: canonical origin records and interning only. Claim: unassigned.
