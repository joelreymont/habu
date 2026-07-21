---
title: Bound profiler counter storage
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-19T20:15:02.741902+02:00\""
---

Frozen review f7ed6085, major correctness and data-integrity defect. src/habu/prof.f:9 reserves a fixed 65536-byte high-data band, only 8192 cell counters, while src/habu/layout.f:66 permits DICT-CAP 32768. BPROF-ON at prof.f:105-117 zeros NDICT cells and EMIT-PROF/EMIT-PROFDUMP at prof.f:14-67 index the same array by every dictionary slot. At NDICT 8193 the zero loop writes index 8192 exactly at DATA plus DATA-SIZE, outside the mapping; up to 32768 is otherwise legal. Independently, habu1.f:1508-1531 lets the user DP grow through DATA plus DATA-SIZE without reserving the hidden profiler band, so valid allot data can be silently zeroed or incremented by prof-on. Root cause: profiler storage is an unowned magic tail, not part of the shared layout contract. Fix: derive counter bytes from DICT-CAP, declare the reserved interval in layout.f, cap DP below it, and make every profiler loop fail closed against the same capacity; mirror bootstrap/cg/prof.fs and bootstrap layout. Acceptance: exact-cap dictionary profiles safely; cap-plus-one is rejected by the dictionary gate before profiling; DP ending at the profiler boundary succeeds and one byte beyond rejects without corruption; snapshot/AOT data never overlaps counters; both targets, profiler gate, layout mutations, fixpoint, size map, and full gates pass.

Claim: agent=profiler workspace=.jj-ws/fable-profiler machine=spark (owns BOTH profiler dots - counter-band bound + pre-attribution exit - one landing closes both; src/habu/prof.f + tests + CODELEN same-commit)
