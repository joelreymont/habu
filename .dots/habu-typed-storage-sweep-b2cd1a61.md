---
title: "Typed storage sweep: 75 now, three view extensions"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T10:05:19.384331+02:00"
---

Phase 3 of 4fd12d60, class A (142 sites): 75 are plain VAR @/! on pre-hook variables - migrate the declaration to TYPED-VARIABLE, no extension needed. Three layout-buffer extensions cover the rest: ext-1 span-valued TYPED-VARIABLE (N-cell value read/written as a unit, ~20 sites: enum-decl.f:152-153 PEND!/@ pair, structure-decl.f:126-127, generated-declaration.f:443-446, hide.f BFR-*); ext-2 field-at-offset over an externally-owned base (data-base/DATAB/pointer var, ~16 sites: xref.f SEAL-NDICT@, env-base, debug, snap-lib); ext-3 indexed row view with per-field types INCLUDING quotation-typed cells (~19 sites: declaration-transaction.f ROW.COMMIT ( ptr a -- ptr [ n -- n ] ), xref.f WATCH-AT, gpt2-model M-SAVE/TAKE). 16 machine-code-emission sites are NOT this class - they stay sealed prims. Blocks the final deletion.
