---
title: Builder TRUST rows to CHECKED
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:54:40.827175+02:00"
---

Convert the ~307 TRUST rows asserting builder emit-word effects (91 in src/habu/habu2.f, habu1.f, jit.f - e.g. habu1.f:7,67,75,251, habu2.f:145,1074,2294,2467) into real checked definitions: the builder is ordinary host Forth over the asm DSL, and reg/label/asm roles already exist (src/core/roles.f). Work file-by-file (habu1 -> habu2 -> jit), keeping the byte-for-byte fixpoint green after each batch; any word the checker cannot yet express gets classified (see habu-trusted-inventory-classifier output) and a named capability dot instead of staying silently trusted. Metric: TRUST row count monotonically down, tracked by the inventory ratchet. Conflicts: src/habu/habu2.f owned by engine worker until CP-rollback lands - start after merge.

## Audit refresh (2026-07-06, head 1eb3b5d3)

Count drift (tools/trusted-inventory.f is authoritative): TRUST rows now 356
repo-wide — habu2.f 101, habu1.f 40, jit.f 5. The counts GREW since mint (engine
work keeps adding builder rows); no conversion batch has landed. The metric and
plan stand unchanged.

## Adopted rows (2026-07-06 pool-dot close)

Owner-of-record for three habu1.f builder-emit rows previously owned by
habu-pool-children-die-6e57e753 (closed - its reaper work is done, the emitter
boundaries persist): `linux-setpgid-self`, `spawn-darwin-zero-attr`,
`spawn-darwin-attr-defaults` (landed with the setpgid prim + spawn
group-leader change, 1ce2fb46). Same builder TRUST->CHECKED conversion class
as the rest of this dot's scope.
