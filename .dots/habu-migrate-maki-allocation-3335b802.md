---
title: Migrate Maki allocation callers
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:14:22.899671+02:00"
blocks:
  - habu-migrate-mem-numeric-8b11a9be
---

Full context: exact Maki direct callers of legacy MEM-ALLOC-BYTES are maki/eval-repair-mech.f, maki/eval-transcript.f, and maki/onnx/import.f. Fix those files and existing focused tests only to validate typed byte lengths and call MEM:ALLOC-BYTES. Acceptance: raw/zero/overflow cases fail before allocation; eval transcripts and ONNX imports remain exact; fixed-string census proves all three calls migrated. Depends on packaged MEM owner.
