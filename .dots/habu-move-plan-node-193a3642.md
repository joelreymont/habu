---
title: "MOVE plan node: unify existing movement lowerings"
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T13:18:59.509137+02:00"
---

docs/tma-gather.md missing-piece-1, dependency-order step 2. Introduce MOVE node (dst tile, src span|idxctx, staging) in the memory plan; refit existing predicated-load and cp.async emitters behind it. NO new lowerings in this dot. Acceptance: sm_87 emitted PTX byte-identical (gate-proven). After 'sm_121a process target row'.
