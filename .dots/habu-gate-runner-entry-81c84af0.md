---
title: gate-runner-entry standalone load dies rc 77
status: open
priority: 2
issue-type: task
created-at: "2026-07-08T09:17:14.697236+02:00"
---

bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f -- <phase> (the exact usage GR-USAGE documents, test/gate-runner-lib.f) dies rc 77 printing a single ':' byte. Bisected via require-prefix files: the first red require is tools/check-core.f when loaded after the 41 preceding gate-runner-support requires (check-core alone fails only E-UNDEFINED CLEANUP-RUN rc 70 = missing prereqs, so the trigger is an interaction, likely check-core's load-time CHK-CHECK-HOOK set-check install rejecting a later gate-runner-support definition). The gate itself never spawns gate-runner-entry (no references in run-lib/run-worker*/run-resident; phases run as resident forks via run-worker.f), so the gate stays green and this is manual-slice tooling debt. Also note the 1-byte ':' diagnostic despite the de-masked top-level throw reporting (44efc694) - the failure path deserves RCA for its silence as well as its cause. Repro artifacts: build a prefix file of gate-runner-support requires and load it; red appears when tools/check-core.f is appended.
