---
title: "Delete the checker's seeded-signature intake"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T12:47:01.294284+02:00"
---

Problem: no engine publishes a signature pool (src/habu/layout.f:391; SIG-LEN-CELL is written only as 0, src/habu/linker.f:5209, :5829), so CK-AOT-READY? (src/core/checker.f:23585) answers false and everything behind it is unreachable: the intake at checker.f:23570-23625, the REG-EXT-AOT-*-XT defers, and type-family.f's REG-AOT check tree (REG-AOT-VALIDATE, REG-AOT-CHECK, REG-AOT-STATE with its fresh/present split and 'seeded registry stores disagree' refusal at :3504, :3523-3527, which only the deleted REG-AOT-TAKE needed). Found by the census review (carl-layout/.out/briefs/review-census.md, M1). The capture's producer (aot-capture.f S-SIGS/S-SIGSTR/S-REG, aot-file.f:82-84, MERGE-REG) stays with the x86 capture, which tools/native-emit-x64.f still reads. Acceptance: the consumer words, defers and checks are deleted, each kept word named with its live caller; no comment names a deleted word; hb builds both generations and the checker and type-family suites pass. Depends: the checker cleanup stack's landing (zvmxsqqm). Claim: unassigned.
