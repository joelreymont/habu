---
title: "A program can store into a DYNAMIC-BUFFER's NAME#base and NAME#ctl cells"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:38:32.541216+02:00"
---

Problem: a program can store into a dynamic buffer's NAME#base and NAME#ctl cells under --load and so defeat the bounds check (src/core/dynamic-storage.f ADDRESS :145): after `1000000 TB#ctl 3 cells + !` a program stores 800 bytes into a 64-byte capacity, rc 0. The definers generate the cells as ordinary source (DBUF-SOURCE src/core/layout-buffer.f:533, run by LBUF-EVAL :157), so they land in the section open at the declaration and resolve like any word: from the declaring package always, from others when the buffer is public (`DBQ:TB#ctl`). tools/check.f refuses the same source (E-UNDEFINED-TOP-LEVEL, E-UNDEFINED) only because CHECKER-DEFDYNAMIC-BUFFER (src/core/checker.f:15465) records NAME, NAME-RESERVE and NAME-RELEASE and never the cells, so load and check disagree. TYPED-BUFFER TT#base, LAYOUT-BUFFER LB#base, CWORK-BUFFER CB#base and public CWORK:MEM#ctl admit the store the same way. Probes and logs: ~/.cache/tmp/carl-abuf-probe, ~/.cache/tmp/carl-ctl. Not probed: DEFER-LAYOUT-BUFFER's NAME#base; a store past the mapping's page; Wasm. Sources that name a cell outside the definers: src/compiler/workspace.f:31, :34, :36, :89; src/habu/t0.f:1580; test/wasm/storage.f:91, :102; test/dynamic-buffer.f:88-203. DYNAMIC-STORAGE:PUBLISH (:160) and ADDRESS are public and take the two cells. The comment above CHECKER-DEFDYNAMIC-BUFFER (checker.f:15425) and one in src/habu/verify-source.f say three published words where five are.

Fix (lead, to design): the generated cells are not words checked source can name, in the declaring package included; only the definer's generated accessors reach them, so load and check agree. The callers above get what they read another way (a null test on the head; the record handed to DYNAMIC-STORAGE).

Acceptance: the cells are private to the definer's generated words and a rejected program proves it, in every mode. Depends: none. Claim: unassigned.
