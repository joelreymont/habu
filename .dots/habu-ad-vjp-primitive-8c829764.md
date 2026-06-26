---
title: "AD VJP: primitive table with per-entry gradcheck"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:00:43.559705+02:00"
blocks:
  - habu-ptx-ad-device-2b511851
---

Decomposes ad-reverse. The VJP: table for the M6 forward primitives (autograd.md VJP-registration + Full-VJP-table): +., -., *., /., SCALE, B-, B/, FMA., EXP., BLOCK-SUM<->BROADCAST, BLOCK-MAX select, LOAD<->STORE, DUP<->+., OVER (fan-out: SUM the two cotangents, NOT a permutation - the review-corrected entry), DROP (zero of the dropped value EXACT type). Each entry is a hand-written backward (the thing ML most fears) and is NOT trusted until it passes the gradcheck harness.
- Files: src/arch/ptx/vjp.f (the paired-word table); each entry carries a T{ }T plus a gradcheck fixture.
- Verify: every entry passes device finite-difference gradcheck; the OVER and DROP type-direction fixtures pass.
- Dep: gradcheck harness (habu-ptx-ad-device-2b511851) + BLOCK-MAX select + BROADCAST.
