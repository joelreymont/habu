---
title: Migrate string numeric roles
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T14:14:22.907725+02:00\""
blocks:
  - habu-seal-cad-num-36dbeec6
---

Full context: lib/string.f uses raw n for byte lengths, offsets, and item counts. Fix: add package STR APIs exactly as MODEL-CAD-V2-PLAN.md B5 lists for LENGTH, OFFSET, COUNT, FIND-SUB, INDEX-OF, SPLIT-NEXT, BUF-RESET, BUF-LEN@, and BUF-APPEND; only owner-internal calls/test move, while legacy ptr-u8-n globals remain an explicit separate caller boundary. Acceptance: empty strings/needles preserve zero, first/last/not-found cases, offset overflow, length/index/offset swaps reject, option<CAD-NUM:index> certifies. Files: lib/string.f, lib/string-test.f. Depends on sealed CAD-NUM arithmetic.

Claim: agent=strnum workspace=.jj-ws/fable-cadnum
