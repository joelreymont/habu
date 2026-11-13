---
title: Captures the waits census missed still pass a deadline
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T20:51:23.700678+01:00"
---

Problem: the census (rest.md section 2) lists no row for capture calls given a timeout in tools/hb-cli-contracts-test.f:142-230 (HCT-TIMEOUT-MS), tools/imagedisasm-test.f:119 (IMDT-TIMEOUT-MS), src/lib/queue-test.f:169 (Q-CAPTURE-MS) and src/lib/os-cpu-test.f:30 (CAPTURE-MS); task-test.f:612 was a fifth, found by lane R2. Acceptance: each passes PROCESS:WAIT-FOREVER or is a deadline its case asserts; a sweep of test code for capture and run calls given a ms finds none unclassified. Depends: the waits campaign commit. Claim: unassigned.
