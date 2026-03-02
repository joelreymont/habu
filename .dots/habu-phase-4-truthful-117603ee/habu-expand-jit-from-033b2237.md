---
title: Expand JIT from real unsupported nodes
status: open
priority: 2
issue-type: task
created-at: "2026-04-01T22:06:02.382774+02:00"
blocks:
  - habu-define-canonical-workload-ae1f969c
---

Problem: JIT coverage expansion must follow measured unsupported-node distribution, not guesswork. Acceptance: new JIT work is driven by canonical unsupported-node evidence from clean workloads. Files: src/jit/** and measured unsupported-node reports. Verify: truthful coverage deltas on canonical workloads. Blockers: habu-define-canonical-workload-ae1f969c.
