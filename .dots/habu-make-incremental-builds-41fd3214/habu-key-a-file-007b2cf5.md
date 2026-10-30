---
title: Key a file by the origin root its rows use
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T19:49:44.520910+02:00"
---

Problem: dc FILE-KEY (tools/native-definition-cache.f, about :1078 at R4 eaed41b6) hashes the engine root only when the origin sink at the file's open is FULL. A file that opens under EARLY and stores FULL rows later (src/compiler/native-runtime.f once fix lane R2 lands) is keyed without the root those rows were located against (src/habu/engine-origin.f:74-97); its close-time key differs from its open key and dc reports ambient-changed (soft today: o:485-497 writes it to stderr; shown for native-runtime.f in carl-fr4/.out/l11/p/warmdiag.log:15937). Found by the R4 review. Requirement: a file's key covers the origin root whenever any of its segments harvests FULL rows, decided by the segments it stores, not the sink at open. Acceptance: no ambient-changed line for native-runtime.f in a warm P0 diag run with R2 and R4 merged; warm == cold. Files: tools/native-definition-cache.f. Verify: P0 diag on the lane 11 tip. Depends: R2 and R4 merged on the lane 11 tip. Claim: carl (lane 11).
