---
title: "Grow the test runner's child capture"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T11:36:09.733942+01:00"
---

Problem: src/lib/test/runner.f:23-33 captures a test's child into fixed 32 KiB buffers (GT-OUT-BUF, GT-ERR-BUF), so a child that writes more throws E-PROC-TRUNCATED out of the whole test ('nothing caught a throw of code -2504') instead of letting it check the output: test/program-diagnostic-long-row.f writes 33 KB of rejection to stderr. process.f already has a growing capture (RUN-STDIN-GROWING-BOTH-CAPTURE-ENDING-LOOP, used by src/lib/process-command.f:488). Acceptance: GT-OUT$ and GT-ERR$ answer a child's whole output whatever its size, through every runner capture (runner.f GT-RUN and GT-CAPTURE-ACTION, test/gate-common-lib.f GE-RUN-ENV, GE-RUN-STDIN, GE-CAPTURE-ACTION and GE-EVAL-FORK-CAPTURE, test/gate-dictionary-lib.f CHECK-BUF-RUN and CANDIDATE-STORE, src/lib/test/assert-test.f:141, test/diag-position-test.f:129); no fixed capture size is left there. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w215.
