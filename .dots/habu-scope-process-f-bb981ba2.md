---
title: "Scope process.f's capture words in a package"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T11:22:13.779260+02:00"
---

Problem: src/lib/process.f keeps its capture machinery as PROC-* globals (PROC-PID, PROC-CAPTURE-PID!, PROC-EXIT-FD, PROC-EXIT-SEEN?, PROC-CAPTURE-MORE?, PROC-KILL-TREE callers) that test/gate-common-lib.f, tools/build-fixpoint.f and the gate pool call directly; the timer lane 1 review (2026-10-30) found two adopters bypassing PROC-CAPTURE-PID! because the state is reachable as bare cells, and the card's package rule would make them private words of a capture package, blocked today by process.f's own global callers. Acceptance: the capture state and its transitions live in one package whose public words are the only way to adopt a pid, read exit state or end a capture; the bare cells are private; every adopter goes through the public words; names are Joel's (names interview). Files: src/lib/process.f, src/lib/process-env.f, test/gate-common-lib.f, tools/build-fixpoint.f, test/gate-pool.f. Verify: rg -n 'PROC-PID !' outside src/lib/process.f prints nothing; hb --load src/lib/process-test.f, src/lib/process-env-test.f, test/gate-common-test.f, test/capture-tree-test.f rc 0. Depends: habu-remove-the-test-6fc078db (lane 1 lands first). Ownership: src/lib/process.f. Claim: unassigned.
