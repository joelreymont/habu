---
title: Run tests on the shipped hb without child processes
status: open
priority: 1
issue-type: task
created-at: "2026-10-30T20:40:12.627548+02:00"
---

Problem: Joel 2026-10-30 ~20:45, after the c2-records hang: 'we decided to build everything in a single binary, why are you launching processes? are you trying to prove that the program you build runs?' then 'no pre-existing! fix this and look for similar code that should be fixed too'. Observed: test/c2-records-e2e.f (and c2-field-loan-e2e.f, c2-memory-e2e.f, c2-view-record-e2e.f) run each case file as a child of hb-root or hb-ordinary, variants from the suite's compiler-images bundle (test/compiler-images.f, test/compiler-images-build.f, test/c2-image-pair.f; deleted with its builder in 54ef9c0c31ee). test/c2-records-lifecycle.f HALT-RESULT :39 spins on READY with no end when the worker throws first; three hb-root children spun 33-93 min (pids 6374, 44605, 69534). Many tests spin on a flag the same way (rg 'while TASK:PAUSE repeat'). Plan: census, lead design from it, one lane per fix. Acceptance: every case runs in the suite's loaded process on the shipped bin/hb unless a process boundary is its subject (image save and restart, exec, exit code, signal, tty/stdin, process state fixtures cannot reset), stated beside it; no test wait outlives a worker that ended. Files: per child. Verify: per child; full suite at landing. Depends: none. Ownership: test harness process use. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-dots (lead).
