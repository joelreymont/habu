---
title: "Drop the arena's slot fallback and state cell alignment once"
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T06:51:57.608356+02:00"
---

Problem: since LE:U64@ became one CELL-VIEW load at any address, src/compiler/ir/arena.f's NATIVE-PROBE/NATIVE-CELLS? (:221-230) is true by construction and its CDIGEST:SLOT@ fallbacks in CELL-AT, IDX-AT and RD-FIND (:236, :472, :501-503) never run; the fact that every target's cells are eight little-endian bytes read at any alignment is repeated in src/lib/le.f:22-23, src/core/bytes.f:25-26, src/core/checker.f:1006-1007 and src/lib/string.f:93, and docs/forth.md:2276-2277 says a cell-typed reader refuses misaligned storage, which CELL-VIEW does not. Acceptance: the fallbacks and the probe are gone; the fact is stated once in docs/forth.md and the card, and each code site keeps only what it hides; forth.md's refusal clause names the reader that refuses or is dropped; warm equals cold. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-cell.
