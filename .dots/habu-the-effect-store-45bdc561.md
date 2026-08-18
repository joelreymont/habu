---
title: "The effect store is 30x bloated: audit and slim it"
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T19:52:30.439112+02:00"
---

User arithmetic 2026-08-18: 7.56MB effect-store growth / 6798 words = ~1.1KB/word for signatures that carry ~8 small integers of information - the store holds garbage with signatures mixed in. MEASURE FIRST (the composition): walk the UEND arena classifying bytes into (a) final signature rows, (b) DUPLICATE rows (13974 arrivals for 6798 words - the engine re-records every declared sig at publish, append-only newest-wins), (c) body-check WORKING nodes (per-token intermediate stack-state effects allocated in the same arena, reclaimed only at scope pops, not per definition - each word may leave its checking transcript behind), (d) orphans. Trace 3-5 representative words' per-definition growth to confirm. THEN THE FIXES, by measured share: per-definition working-node reclamation (the rollback machinery exists - a definition-scoped cursor rewind for nodes that never escape the check); replace-not-append for re-recorded declared sigs; row/subterm interning (hundreds share ( n -- n )). This is the DATA-WINDOW DISEASE ONE LAYER DOWN (working state persisted as results - the user's buffers-at-startup ruling applied to the checker's own arena) and it also: shrinks every process by ~7MB, relieves the arena-cap pressure paid all week, and makes the binary type-info encoder's source material clean. Acceptance: the composition table published; UEND growth for the chain load reduced by the measured garbage share with the fixes attributed row by row; all checker gates green (the certify census is the behavior pin).
