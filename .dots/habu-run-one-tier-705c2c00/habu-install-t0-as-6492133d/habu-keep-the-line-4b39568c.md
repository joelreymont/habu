---
title: "Keep the line in an x86 refusal's place"
status: active
priority: 2
issue-type: task
created-at: "2026-11-01T20:05:12.027566+02:00"
---

With T0 as x86's tier 0, a refusal names the file without the line: '0 set-check : GEKT ( n -- n ) begin then ; 5 GEKT .' prints 'ncomp: cannot compile GEKT at then in <dir>/kt.f' on the T0 x86 stack's engine, rc 70; arm64 T0 prints 'kt.f:2'; master's x86 tier 1 printed 'at ; in kt.f:2'. ': GEKT ( -- ) nosuchword ;' has no line on master's x86 tier 1 either. Shows as engine-runtime-regressions GE-CF-KIND 'FAIL: then' on x86 (found by C4b fold-in B, jerry-t0x86jit). Root-cause and fix lane: worker in .jj-ws.noindex/jerry-t0x86loc on C4c, brief /tmp/jerry-big/t0x86-loc-brief.md. Claim: jerry.
