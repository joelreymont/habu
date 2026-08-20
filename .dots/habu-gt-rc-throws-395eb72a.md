---
title: GT-RC= throws away the evidence
status: open
priority: 2
issue-type: task
created-at: "2026-08-20T23:00:32.341000+02:00"
---

From the incident's send-back round (2026-08-20): lib/test/runner.f:309 GT-RC= reduces the observed exit code to a bool before GT-CHECK, so a failing runner assertion logs 'TFAIL runner 1 ok rc' with the actual rc never printed - two agents diagnosed spawn failures blind because of it. Fix: GT-RC=/GT-STDOUT=/GT-STDERR= report expected-vs-got the way T= does. Build-the-tool rule, small change, own gate.
