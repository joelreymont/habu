---
title: cad-replay-test is load-sensitive
status: open
priority: 2
issue-type: task
created-at: "2026-08-15T18:13:53.826821+02:00"
---

maki/cad-replay-test.f fails under concurrent load and passes idle: 6 concurrent instances -> 4/6 RC=1 with TFAIL asserts (108/109 'expected true got false' and downstream), reproduced IDENTICALLY on unmodified master (scratchpad control tree) and on the bake stack - pre-existing, not stack-specific. Unloaded: 12/12 green across both trees. Suspect the 120s child timeout (the test spawns the whole cad-test suite under a cloned engine, maki/cad-replay-test.f:34 SUITE-TIMEOUT-MS) or a timing-sensitive assertion under contention. It reds test/run.f's maki core phase whenever the box is busy - a false-red generator for every merge gate. Diagnose with evidence (which assert flips, what the child actually did), fix the root cause: no timeout bump without proof the timeout is the mechanism.
