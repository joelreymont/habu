---
title: "r3b: fix the 8 stack reds on candidate yorztvvy"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T21:09:40.592855+02:00"
---

Suite /tmp/jerry-big/r3b-suite.log, candidate yorztvvy (ctrl excluded): 500 of 500 rows, 8 red: check-cli-boundary, check-verify, c2-memory, outer-interpret, pg, program-diagnostics, engine-runtime-regressions, native-file-replay (R3 had 33). Master status on public hb (= master@origin tree) in .jj-ws.noindex/jerry-mstat: check-cli-boundary green, check-verify green (both stack regressions); engine-runtime-regressions is the ctrl lane's rewritten RXSC rows (lane habu-ctrl-refuse-malformed-65c26004). Rest pending.
