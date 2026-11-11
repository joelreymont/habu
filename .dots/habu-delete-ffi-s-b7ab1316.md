---
title: "Delete FFI's declaration observer, which nothing sets"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T09:49:27.210914+02:00"
---

Problem: src/lib/ffi-abi.f keeps an observer cell (OBSERVER :235, NOTIFY :242, OBS-LIB/OBS-FN/OBS-SCOPE, public OBSERVE, UNOBSERVE, NOTIFY-SCOPE, OBSERVER-CELL :1000-1004, the cell's span at :1044) and calls NOTIFY from LIBRARY-PATH, DECLARE and the scope recorder, but no file in Habu, Zen, Tender or Maki calls FFI:OBSERVE, FFI:UNOBSERVE or FFI:OBSERVER-CELL (rg -i --no-ignore over the tree and the three consumers' masters, 2026-11-11), so the cell is always 0 and every NOTIFY does nothing. It is a hook of the kind AGENTS.md lists as machinery being replaced. Acceptance: the cell, its casts, the three kinds, NOTIFY and its call sites, the four public words and the cell's span are gone; comments that name the observer are corrected; nothing else changes. Files: src/lib/ffi-abi.f. Verify: engine built twice, src/lib/ffi-test.f, then the landing suite and chain with the batch. Depends: habu-zip-asks-linux-88bcf59f's commit (same file; base the lane on it). Ownership: carl. Claim: unassigned.
