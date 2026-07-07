---
title: "Stdlib: check@ hook getter"
status: open
priority: 2
issue-type: task
created-at: "2026-07-07T20:17:26.422031+02:00"
---

From the reentrancy mechanism RCA (habu-checker-reentrancy-certify, 2026-07-07): there is no word to READ the active check hook - set-check writes [x20/DATA + HOOK-CELL B0] but nothing reads it back; an agent reached for dbase@ (x26/DBASE - a CODE region) and installed 8 bytes of code as the hook, producing the layout-unstable does>/hang/SIGBUS family. If hook save/restore is ever genuinely needed, add a typed check@ ( -- xt ) reading [x20+B0] (mirror BSETCHECK habu1.f:1659), with a round-trip regression (check@ set-check = no-op) and a negative test that a garbage install still dies with a NAMED diagnostic rather than a BLR into code (consider validating the xt range in set-check itself - fail-closed at install beats crashing at publish). Low priority: no natural path needs the swap idiom today.
