---
title: "Tick internal words at tier 0 in TRUSTED:"
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T19:36:44.687594+03:00"
---

Problem: tier 0 and tier 1 disagree when a TRUSTED: body ticks an internal engine word (`DNAME-INT`). NCOMP admits it: `src/compiler/native/dict.f` `VISIBLE-RECORD?` and `CALL-BINDING` admit `DNAME-INT` while the TRUSTED: compilation cell is armed. Tier 0's compiled tick (`src/habu/habu2.f` `C-BTICK`) refuses it, though the batch-13 tick fix taught it to admit trusted-only words in TRUSTED: bodies. Ticking adds nothing a direct call in a TRUSTED: body lacks (`docs/type-families.md`). Found on the tick fix lane, batch 13.
Acceptance: in a TRUSTED: body, `['] W` of a `DNAME-INT` word compiles at tier 0 as it does at tier 1. A checked body and an interpret-level tick stay refused at both tiers. Add cases beside `TICK-TRUSTED-BODY` in `test/outer-interpret.f`: admitted at both tiers in TRUSTED:, refused at both in a checked body.
Files: `src/habu/habu2.f` (`C-BTICK`), `test/outer-interpret.f`.
Verify: outer-interpret; spark chain gen2 == gen3 (habu2.f is baked); full gate.
Depends: none.
Ownership: krait.
Claim: unassigned.
