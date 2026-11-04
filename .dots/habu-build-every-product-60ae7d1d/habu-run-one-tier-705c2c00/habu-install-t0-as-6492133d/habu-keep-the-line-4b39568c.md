---
title: "Keep the line in an x86 refusal's place"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-01T20:05:12.027566+02:00"
closed-at: "2026-11-02T11:27:57.681339+02:00"
close-reason: "Landed with the T0 x86 stack: nmwplslm 'Write body source rows in x86 body-append' gives x86 body-append its BODY-ROW source rows, so a T0 refusal on x86 names file:line as arm64 does; engine-runtime-regressions GE-CF-KIND 'FAIL: then' no longer appears on the pulsar x86 run (/tmp/jerry-big/x86stack2/suite.log)."
---

With T0 as x86's tier 0, a refusal names the file without the line: '0 set-check : GEKT ( n -- n ) begin then ; 5 GEKT .' prints 'ncomp: cannot compile GEKT at then in <dir>/kt.f' on the T0 x86 stack's engine, rc 70; arm64 T0 prints 'kt.f:2'; master's x86 tier 1 printed 'at ; in kt.f:2'. ': GEKT ( -- ) nosuchword ;' has no line on master's x86 tier 1 either. Shows as engine-runtime-regressions GE-CF-KIND 'FAIL: then' on x86 (found by C4b fold-in B, jerry-t0x86jit). Root-cause and fix lane: worker in .jj-ws.noindex/jerry-t0x86loc on C4c, brief /tmp/jerry-big/t0x86-loc-brief.md. Claim: jerry.
