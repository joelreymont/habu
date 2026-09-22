---
title: "native-window-owner.f is red standalone: its child needs the whitebox engine"
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T09:40:06.049306+03:00"
---

Problem (measured on 804041a8 at 419e2ba6): bin/hb --load test/native-window-owner.f standalone ends 'test: failures' rc 1 - its child answers 'hb: internal engine word: DECLARATIONS' (exit 70) and the 'window: 0' assertion then fails; the gate row WHITEBOX-SUITE native-window-owner (test/gate-stdlib-cases.f:465) is green because SUITE-WB-RUN hands it the unsealed engine. Same class as cf352fb7 (native-checker-prefix.f, landed 287c5810): the file only spawns. Acceptance: the file provides its own engine through WHITEBOX-ENGINE:PROVIDE under its own temp root the way native-checker-prefix.f does, sets HABU_UNDER_TEST/HABU_FIXPOINT_ENGINE for the child, its row becomes a plain SUITE, standalone test: ok rc 0; the remaining WHITEBOX-SUITE rows are swept once (rg -n WHITEBOX-SUITE test/gate-stdlib-cases.f): every file that only spawns gets the same shape in the same commit, files that reach inside the engine themselves stay WHITEBOX rows, and docs/gate.md's bullet stays true. Files: test/native-window-owner.f, test/gate-stdlib-cases.f, any sibling found. Depends: none. Ownership: hazel (gate/test load path). Claim: unassigned.
