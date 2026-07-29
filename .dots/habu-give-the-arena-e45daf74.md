---
title: Give the arena a rollback epoch
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T17:54:45.298540+02:00"
---

Full context: DEFECT found by the Rocq storage proof and CONFIRMED live against bin/hb. IR-ARENA:ROLLBACK truncates the cursor and nothing else (src/compiler/ir/arena.f:323-330); it does not bump the arena generation, and an index carries only (generation, ordinal). So an index minted above a mark can come back to life over a DIFFERENT cell. Proven: push 11, mark, push 22 keeping its index, roll back, push 99 — PEEK on the kept index answers 99 with no diagnostic. This is a classic ABA. test/compiler/ir-arena.f covers the case where the cursor stays below the index (MR-DEAD gives E-IR-ARENA-BOUND) but never the case where a later push passes it. Fix by giving IR-ARENA a per-arena rollback epoch so an index or mark minted above a mark cannot be re-validated after the cursor passes that point again — either bump the arena generation on rollback (invalidating every index, simplest and strongest) or pack an epoch field beside the ordinal. Regression: the ABA sequence must throw a named code instead of answering 99, and the Storage.v example arena_rollback_reuses_ordinal becomes a theorem that it is refused.
