---
title: Derive census walk set from FILEMAP
status: open
priority: 3
issue-type: task
created-at: "2026-07-26T09:02:20.574716+02:00"
---

Problem: tools/enum-census-core.f hardcodes the walked tree list (WALK-TREES, line 494: src, lib, tools, test, maki) and pins WALKED-FILES 1258 by hand; every file addition moves the count and the guard text tells the operator to update the constant. FILEMAP.md is already the authoritative file inventory with its own lint. Required result: derive the walked file set from FILEMAP.md so the census walks exactly the tracked Forth sources; the loud guard remains but compares against the FILEMAP-derived count, so a tree missing from the walk is structurally impossible and a hand-maintained constant disappears. Depends on the one-entry-per-line filemap-lint shape fix so a corrupted FILEMAP cannot silently shrink the walk; fail closed if FILEMAP does not parse. Acceptance: the census walks the same file set as today on the clean tree (proven by identical file count and identical baseline); deleting a FILEMAP entry for an existing file reds the census walk; the hardcoded tree list and count constant are gone. Files: tools/enum-census-core.f, tools/enum-census.f. Verify: the census verify run against the committed baseline. Depends: habu-enforce-one-filemap-3d42ccbe. Ownership: census file-walk derivation only. Claim: unassigned.
