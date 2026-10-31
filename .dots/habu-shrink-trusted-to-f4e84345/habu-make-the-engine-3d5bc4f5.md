---
title: Make the engine build refuse what a source load refuses
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T13:28:26.209522+02:00"
closed-at: "2026-10-31T13:37:24.810589+02:00"
close-reason: "Root cause, no defect: the arm64 engine build never loads src/habu/main.f (native-runtime.f LOAD-HABU-LOOP requires runtime-x64.f, the only loader of main.f, interpret.f and definers.f, only for linux-x86-64), so its check is not weaker, it is not applied. The x86-64 cross-build ($HOST --load src/arch/x86-64/passes.f tools/native-build.f -- .out/x64/hb --target linux-x86-64) refuses REPORT-PTR as : with the source load's text, rc 74. A TRUSTED: conversion in an x86-64-only loop file is proven by that cross-build or a whitebox row that source-loads the file (main-argv). Answer /tmp/jerry-big/bvr-answer.md. Follow-ups: habu-correct-the-interpreter-b60e605f, habu-answer-tools-check-745b0ccd."
---

Problem: main.f REPORT-PTR as : (data-base ENGINE-MAIN:REPORT-CELL + typed ptr [ n -- ]) passes tools/native-build.f but a source reload on the unsealed engine refuses it (raw storage cell cannot hold an execution token, FENCE-EXEC). The build is the only proof for engine-provided files, so its check must not be weaker. Acceptance: root cause named; the build refuses it the same way, with a test failing without the fix; any other engine body the fixed build refuses listed. Files: per root cause. Verify: engine build; named rows. Depends: none. Ownership: per root cause. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-tp2
