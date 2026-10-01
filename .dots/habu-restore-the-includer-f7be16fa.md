---
title: "Restore the includer's using slots after a throw"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T13:11:50.441672+03:00"
closed-at: "2026-10-01T14:07:22+03:00"
close-reason: The eval frame, the Habu loop and the REPL put the includer's slot wids back and the checker names them; using-test, outer-interpret and proc-pty pass on gen1=gen2 95fe3be5.
---

Problem: after a caught throw, a buffer that closed its includer's package and then opened a using leaves that using in the includer's slot. Lane UI (habu-keep-the-includer-bd7dc952) landed a per-buffer using floor (EVAL-USE-FLOOR, frame cell $98; OUTER USE-FLOOR; CHECKER-USE-SOURCE-FLOOR) that fixes the clean path and refuses a buffer's ;using below its floor, but the throw restore still puts back only depth and floor. Restoring the engine's slots alone is unsafe: the checker resolves a bare tail through its names mirror (CK-USE-NAMES, read by CHECKER-USED-SYM), not the engine's wids. Measured on base 75b4 (UI's scratch names-diverge.f): with engine slot 0 = UA and the checker's name for it still UB, where UB:AW is ( n -- n ), ': Y ( n -- n ) AW ;' certifies, yet the call runs UA:AW ( -- n ) and '5 Y' leaves 11 5. The REPL has the same gap: a line ';package using X NOPE' inside a package with usings leaves the slot overwritten after recovery.
Acceptance: one source of truth for slot names. The engine's slot wids are authoritative and the checker re-derives each slot's package name from them (a live provider like PKG-LIVE-XT that maps a wid to its package by the namespace rows), re-read in the throw resync both loops already call (CHECKER-PACKAGE-RESYNC). The eval frame snapshots the includer's USE-MAX slot wids on entry and restores them with depth and floor on a caught throw; the Habu loop's INTERPRET does the same; the REPL recovery restores its line's entry slots. Tests: the throw shape (package PP + using UA; buffer ';package using UB NO-SUCH' under catch; then AW resolves to UA and a checked caller of AW certifies against UA:AW), the names-diverge case refused or consistent, and the REPL line, in test/using-test.f and test/outer-interpret.f.
Files: src/habu/habu2.f (B-EVAL, the throw restore), src/habu/layout.f (frame size), bootstrap/cg/forth.fs if the frame grows, src/habu/interpret.f, src/core/checker.f (the provider and resync), the two suites.
Verify: the suites; chain, gate and Gforth recovery (frame layout).
Depends: none.
Ownership: krait.
Claim: unassigned.
