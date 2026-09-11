---
title: Reuse one compile context and word model per session
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-11T16:38:06.259940+03:00\""
---

Problem: per-definition fixed cost is 16.9 s of the load, 5.49 ms per definition: context mmap/unmap 17 us, HIR builder plus 15 tables 29 us, three A64 builders 53 us, MODEL 2.26 ms (86-declarer walk 6.6 s total), seven dialect-schema binds 3.13 ms (9.6 s total), BIND memos keyed on module identity missing exactly 46 times per definition. Trivial-definition floor 6.8 ms (9.1 ms with a combine rewrite). Acceptance: one IR context and word model per session, session-keyed BIND memos (0 misses on the second definition, test), explicit per-definition arena watermark so a failed definition publishes nothing (test), no growth across 3,079 definitions, IN-CONTEXT prohibition, IMAGE-LIFECYCLE/CAPTURE-PREPARE cleanup and ROUTINE untouched; trivial floor measured before and after; controlled pair. Files: src/compiler/native/compiler.f, hir.f, a64ir.f, hir-word.f and tests. Verify: compiler suites; forced-tier Tender pair. Depends: none. Ownership: rowan, workspace .jj-ws/rowan-reuse (in flight). Claim: agent=rowan workspace=.jj-ws/rowan-reuse
