---
title: "TFAM 12: interpret-mode wide layout values"
status: open
priority: 2
issue-type: task
created-at: "2026-07-06T22:21:10.423427+02:00"
---

Interpret-mode (top-level, outside a colon body) transports of layout/bundle values are reachable TODAY via a TRUSTED maker at the unchecked REPL and SILENTLY CORRUPT instead of failing closed. Repro: TRUSTED: MK ( -- pp<n,n> ) 7 9 ; then top-level  MK dup . . .  prints  9 9 7  (exit 0) — interpret-mode dup copies only the top/tag cell of the 2-cell bundle, not the whole group, and no reject/die fires. This is independent of items 8/9 (constructors) — the gap exists now through any TRUSTED boundary that yields a wide value at the REPL. Mechanism options: (a) interpret-mode width tagging — carry the checker/registry logical width on the interpret data stack so top-level stack ops (dup/drop/swap/...) move whole groups, mirroring the compiled pass-2 lowering; or (b) a top-level check hook that types interpret-mode lines and either width-aware-lowers or fails closed on a wide transport at the unchecked REPL. Acceptance: top-level MK dup either yields the correct 4-cell copy (9 7 9 7) or fails closed with a named diagnostic; a committed interpret-mode regression covers dup/drop/swap over a TRUSTED-seeded bundle. Referenced by habu-tfam-12-layout-057181a9 REMAINING item (2), whose stale 'unreachable' rationale was corrected.
