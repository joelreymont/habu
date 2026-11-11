---
title: "S5b1b: run a build with no host package open"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T23:24:59.923473+02:00"
closed-at: "2026-11-05T23:25:05.919400+02:00"
close-reason: "landed in 'Run a build with no host package open', reviewed twice (last OK); bin/hb /tmp/jerry-s5b1b/p5.f ok5 rc 0; bin/hb --load test/run.f -- linker PASS 1152 ms rc 0 (/tmp/jerry-s5b1b/proof-land.log); mutations: without the PKG-OUTSIDE-CELL store-back l2 fails, OPEN-L throwing 75 fails the RAN-L check (mut2-1a.log, mut2-ranl.log)"
---

Problem: LINKER:WITH (src/habu/linker.f) run while a host package is open refused the body's first definition, rc 70 'no authenticated package context': MOVE-IN pointed current at the space's global wordlist while the engine's package cells still named the host package. Reproducer /tmp/jerry-s5b1b/p5.f. Acceptance: INTERPRETER:WITHOUT-PACKAGE ( [ -- ] -- ) in src/habu/packages.f runs a quotation with no package open and puts the caller's scope and PKG-OUTSIDE-CELL back on every exit; LINKER:WITH runs its body under it; test/linker.f cases k and l (RAN-L proves OPEN-L returned before the part was refused). Files: src/habu/packages.f, src/habu/linker.f, test/linker.f. Verify: bin/hb /tmp/jerry-s5b1b/p5.f (ok5), bin/hb --load test/run.f -- linker. Depends: S5b1. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-s5b1b.
