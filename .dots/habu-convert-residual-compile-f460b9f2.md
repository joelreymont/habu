---
title: Convert residual compile dies; REPL recovery parity
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-16T22:35:46.772792+02:00\""
---

Follow-up from habu-raw-exit-compile-6bf05f98 (landed 76aa0ad7 - the LCOMPILEDIE shared recovery tail now exists, making each additional conversion a small mechanical change): (1) convert the out-of-inventory recoverable die sites to catchable throws via LCOMPILEDIE with the same per-site rollback audit discipline - J-DOES/J-QUOT/J-SEMIQUOT (75), C-LBRACE-STORE-ONE (75), C-SIG-BAD (76), C-DEFER-DIE-TOKEN, C-QUOTE-EOF (74), counted-string >255, postpone/export-undefined; any site whose rollback cannot be made consistent stays die with the reason documented at the site. (2) tty-REPL recovery parity: LUNDEF recovers the interactive REPL but the converted compile errors still exit the tty session (evaluate-only recovery was Option A) - decide and implement whether the REPL loop should catch-and-continue on LCOMPILEDIE codes like it does for undefined words, preserving script/stdin exit behavior exactly. (3) optional: a cheap in-evaluate overflow fixture (DICT-CAP test knob or accept the documented unit-cost proof). Acceptance per conversion: caught-inside-evaluate fixture with exact code+diagnostic+usable-session, top-level byte-identical exit+diagnostic, fixpoint x2, engine batteries, full run.f. Files: src/habu/habu2.f, test/gate-engine-lib.f. Ownership: engine error recovery.

Claim: agent=residual workspace=.jj-ws/fable-residual
