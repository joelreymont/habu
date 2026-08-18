---
title: "The effect store is 30x bloated: audit and slim it"
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T19:52:30.439112+02:00"
---

User arithmetic 2026-08-18: 7.56MB effect-store growth / 6798 words = ~1.1KB/word for signatures that carry ~8 small integers of information - the store holds garbage with signatures mixed in. MEASURE FIRST (the composition): walk the UEND arena classifying bytes into (a) final signature rows, (b) DUPLICATE rows (13974 arrivals for 6798 words - the engine re-records every declared sig at publish, append-only newest-wins), (c) body-check WORKING nodes (per-token intermediate stack-state effects allocated in the same arena, reclaimed only at scope pops, not per definition - each word may leave its checking transcript behind), (d) orphans. Trace 3-5 representative words' per-definition growth to confirm. THEN THE FIXES, by measured share: per-definition working-node reclamation (the rollback machinery exists - a definition-scoped cursor rewind for nodes that never escape the check); replace-not-append for re-recorded declared sigs; row/subterm interning (hundreds share ( n -- n )). This is the DATA-WINDOW DISEASE ONE LAYER DOWN (working state persisted as results - the user's buffers-at-startup ruling applied to the checker's own arena) and it also: shrinks every process by ~7MB, relieves the arena-cap pressure paid all week, and makes the binary type-info encoder's source material clean. Acceptance: the composition table published; UEND growth for the chain load reduced by the measured garbage share with the fixes attributed row by row; all checker gates green (the certify census is the behavior pin).

PHASE-1 VERDICT + RULINGS 2026-08-18 (the table: 51.14% final
rows / 48.86% publish-replay duplicates / 0.00% working nodes -
suspect (c) is DEAD, SIG-EFF-DROP already reclaims, FIX 3 IS
STRUCK from this leaf by measurement; the graph is 83,819
copies of 1,099 shapes, floor 1.07%):
(1) THE PLAN IS APPROVED: fixes 1+2 through the one primitive
(completed row/subterm at top-of-arena; structurally-identical-
below -> rewind UEND, return the older offset) with STRUCTURAL
COMPARE ON EVERY HASH HIT - the lane's rejections of hash-only
(silent-miscompile memo) and text-compare (wrong across scope)
are the ruling's own reasons. Target: 926KB, 136B/word, 8.2x.
(2) PHASE-2 FIRST TASK, before any edit: the reader containment
audit (every consumer of a record's graph checked for the
lives-inside-ER.NEXT-span assumption; the census tool's own
assumption changes with it).
(3) The intern table: generation + truncation on the existing
USX-GEN pattern, entries at/above newend dropped on
USIGS-RESTORE-END, snapshot reset in the chain - approved; the
below-itself reference direction makes rewind safety structural.
(4) ORDERING RULED: this lands BEFORE the binary type-info
encoder (c6a3d0ff) - the DAG is BETTER source material (interned
subterms become the format's interning for free); the encoder
consumes the slimmed store.
(5) The census tool lands as tools/effect-store-census.f with
its three validation windows as registered fixtures - it IS the
acceptance instrument.
(6) Var-id normalization keying (stored ids, never live) -
adopted as stated. NOTE for the record: post-fix 136B/word
still carries the 9-cell node at 2.6 cells full - node packing
is a future format question (rides b659718e's family), not this
fix.

