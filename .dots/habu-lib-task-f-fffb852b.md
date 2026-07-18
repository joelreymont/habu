---
title: "lib/task.f: delete or name its future consumer (~830 lines)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T14:15:40.502122+02:00"
---

Depth review: 512+317 lines pthread tasking, zero production consumers; only test/protection-span.f + test/seal-package.f use package TASK as a SEALING TEST SUBJECT (could seal a 20-line stub instead). Gate parallelism is process-fork, not pthread. V2-plan mentions are layout-coexistence notes, not consumption. Owner call: archive or dot the consumer.
