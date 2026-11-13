---
title: Delete the lowering transaction band
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T18:39:10.467585+01:00"
---

Problem: the DATA band TXN-STATE (src/habu/layout.f) holds TXN-ACTIVE-CELL, never read, and P2-CELL, set only by an x86 test; EMIT-BCAP still tests it for a pass 2 that no longer exists. Acceptance: the band, its cells, its claim and both kernels' P2-CELL test are gone; the build converges and the engine tests pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-fu12
