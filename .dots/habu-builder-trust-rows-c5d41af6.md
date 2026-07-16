---
title: Builder TRUST rows to CHECKED
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-01T22:54:40.827175+02:00\""
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

## Adopted rows (2026-07-11 audit increment 3)

Owner-of-record for the full builder-emit classification surface (186 rows:
every named row in icode/aot-*/build/crash/debug*/habu1/hide/jit/layout/
maker/prof/snap*/stage2/stdin/treeshake/verify-source/xref/image-bytes/elf/
os-layouts/macho/imagedisasm/imgdump/jitdump-core plus the held habu2.f
fold) — reassigned from the audit placeholder with per-file evidence in
habu-audit-trusted-inventory-3a950436. First conversion batch landed with the
same increment: CODE-BYTE+/CRH-BYTE+/XREF-REC+ discharged to checked
(pointer+offset arithmetic now certifies), trust surface 496 -> 493.

BATCH 1 LANDED 2026-07-16 (btrust lane, habu1.f): 40 of 41 habu1.f TRUST rows
removed as PROVABLY REDUNDANT - the build certify pass (verify-source.f
VERIFY-SOURCE ~305) runs CHECK! on every : body and throws on reject AND
uncheckable, so a passing build means every body was already certified against
its declared effect; the trust rows were redundant registrations (per
docs/forth.md: do not keep a TRUST row to pin a CHECK!-passing word's scheme).
Includes the 3 adopted OS-boundary rows and the 5 higher-order rows
(fprim/fprim-l/fprim-wid/emit-prims/emit-fp-prims - the modeled execute and
['] handling certify them). Counts: habu1.f 41->1, repo TRUST 398->358,
unclassified 0, ratchet ok. Binary drift 460 bytes = baked AOT-REPL data
address immediates only (142 MOVZ imm + 3 table offsets, zero opcode changes),
x2 self-reproducing fixpoint 76d67763. Claim released.

RESIDUAL habu1.f row (1): STDIN? ( -- ptr bool ) - a variable, not a : word;
the variable definer yields generic -- ptr a and the checker cannot infer the
bool cell refinement. Conversion path: define via the existing TYPED-VARIABLE
declarator (a definition-form refactor with codegen-drift risk, not a checker
gap) - fold into the habu2.f batch or a focused follow-up.

REMAINING BATCHES: habu2.f (~101 rows), jit.f (~5 rows). Hypothesis worth
testing first in each batch: the same verify-source redundancy argument may
apply to most : -word rows - try mass-removal + build before hand-converting.
