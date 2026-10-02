---
title: Compact the effect store to the kept symbols
status: closed
priority: 1
issue-type: task
created-at: "2026-09-30T18:50:15.052768+02:00"
closed-at: "2026-10-02T10:40:00.000000+02:00"
close-reason: "Landed: retired symbol rows and effect bindings zeroed in place; product 2,344,951 -> 2,179,831 B"
---

Problem: after 974304d0 retires the symbols, their effect bindings, unshared contents and nodes are still in USIGS. Measured on the whitebox store with the product policy: 8,052 of 21,278 records are retired; their headers still carry NEXT, ACTIVE and CONTENT (36 KB of LEB128 values) and their spans 3,574 unreachable present cells (6.6 KB). `UIX-REC-ADD` reads a record's NEXT, CONTENT and rows and nothing else of its span, so a retired binding can be zeroed in place as long as its chain link survives.
Acceptance:
- **Mark-and-zero:** `CHECKER-SWEEP:RUN` marks, from every record of a kept symbol and from the control rows' CREATES, DOESEFF, WRAPC, RECEFF and the PES rows' effects, every content and node (strings and argument runs included) they reach, with the shape walk `UIX-NODE-ADD` and `tools/effect-store-census.f` WALK implement, then zeroes every unmarked 8-byte granule of the user region. A retired binding keeps `ER.NEXT` alone; ACTIVE, SYM, SYMPREV and CONTENT are zero. Nothing moves: no store position, offset, `USIGS-USER-OFF` or `UEND` changes, so no saved position or latch is remapped.
- **Walkers:** `UIX-REC-ADD` skips a record whose `ER.CONTENT` is 0; the census skips its rows and publishes `DEAD-BYTES` (zeroed unmarked granules) so window = final + dup + dead with orphan zero.
- **Equivalence check:** in process, for every kept symbol `USIG-NEWEST`, the record's reachable bytes, the control word, the defer flag, the created effect, `EFFECT-QUERY` and `SIG-MIN-IN` are equal before and after; every retired symbol resolves to nothing and its record is zero but for its link.
Files: `src/core/checker.f`, `tools/effect-store-census.f`, `test/effect-store-census-test.f`, `test/effect-store-sweep-test.f`, `test/gate-stdlib-cases.f`, `docs/engine-size.md`.
Verify: the sweep and census tests on the whitebox engine; `test/run.f`; generations byte-identical; engine-size and data-table-census before/after recorded in `docs/engine-size.md`.
Depends: habu-drop-private-signatures-974304d0.
Parent: habu-ship-only-the-d7d38629. Design: the Fable surface design of 2026-09-30 (~/.cache/tmp/heron-arm64/design-surface.md); census: ~/.cache/tmp/heron-arm64/size-census/.

Landed 2026-10-02 in two commits: uqxmtupx e11128a5 (Zero retired checker symbol rows, in master 55e0b0aa: product 2,344,951 -> 2,229,367 B) and tlxvykyo 5d1e22ad (Zero retired effect bindings in place: RETIRE-BINDINGS marks from kept records and roots and zeroes every unmarked granule; product 2,229,367 -> 2,179,831 B, DATA values -56,448, code and site tables +3,348; census and sweep tests with tampers; docs/engine-size.md). Fable review accepted.
