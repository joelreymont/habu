---
title: "amb: exit as the refusal for an ambiguous native word"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T18:09:27.488974+02:00"
---

Problem: a native body naming a bare word two using packages export: the checker reports E-USING-AMBIGUOUS, then uncaught throw 7144, rc 67 (JIT rc 94); tier 1 prints 'ncomp: cannot compile ' with no name. Acceptance: T0 and tier 1 exit 70 with the report, no uncaught line; catch still sees E-USING-AMBIGUOUS (test/using-test.f); new case in test/compiler/native-hookless-reject.f. Repros /tmp/rev-rows/s/amb-{t0,t1,jit}.f. Brief /tmp/jerry-big/t0-amb-brief.md. Base pvovztnu (uref). Claim: agent=jerry workspace=.jj-ws.noindex/jerry-amb
