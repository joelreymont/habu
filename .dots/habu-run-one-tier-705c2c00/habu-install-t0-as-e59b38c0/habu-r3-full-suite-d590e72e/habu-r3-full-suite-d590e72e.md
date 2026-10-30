---
title: "r3: full suite on the stacked T0 candidate"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T18:41:07.008163+02:00"
---

bin/hb --load test/run.f on mnzvrsym's candidate in .jj-ws.noindex/jerry-stack; log /tmp/jerry-big/r3-suite.log. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-stack

2026-10-30 21:00: stack rebased onto master 6dccbb6a and linearized (itick2 into uzllnlko, uref2 into pvovztnu, lanes in one line, imm below ctrl); lead workspace .jj-ws.noindex/jerry-lead on yorztvvy; eim's leftover PROT-EMIT:LCLOSE call dropped (master removed the close with the two-view mapping). Candidate built; full suite on it (ctrl excluded) running, log /tmp/jerry-big/r3b-suite.log. No x86-64 build (see parent). Claim: agent=jerry workspace=.jj-ws.noindex/jerry-lead
