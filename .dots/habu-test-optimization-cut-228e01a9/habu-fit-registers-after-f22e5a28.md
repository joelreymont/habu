---
title: Fit registers after an eviction without a rescan
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T07:26:02.745051+02:00\""
closed-at: "2026-10-01T17:06:11.585461+02:00"
close-reason: Fixed by uuxpvlxy f75eb8f5 (review 121 ACCEPT)
---

Problem: src/compiler/native/regalloc.f MB-FIT rescans the whole function after every eviction, so allocation costs evictions x positions; test/compiler/native-wide-frame.f's 31040-byte word (about 3600 evictions) takes about 6.5 s CPU to compile at tier 1, making its new gate row slow (found by the r4-nest lane after 071738af made frame pressure linear). Acceptance: after an eviction only the affected range is refitted, so that word compiles in time near-linear in evictions plus positions (measure before and after, with the depth table from 071738af still linear); allocations unchanged: g0 == g1 byte for byte and the masked per-word comparison over the loop and frame suites shows 0 differences; native-wide-frame's row time cut accordingly. Files: src/compiler/native/regalloc.f. Verify: the loop and frame suites 071738af ran, native-wide-frame, native build g1/g2 cmp, two-generation build. Ownership: tier-1 allocator refit cost.
