---
title: "Kernel perf: complete IR watch set"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-15T07:30:08.222046+02:00"
closed-at: "2026-07-21T00:15:56.141923+02:00"
close-reason: "Landed (stack tip 24c19014): the kernel-perf lint's broad prefix watch rule replaced by a checked exact-path canonical producer table (new tools/ptx/perf-watch.f, package PERF-WATCH, 57 producer paths: arch emitters, IR+optimizer, tile DSL, reg/ABI/header, cg codegen, AD lowering, 26 per-kernel drivers) with 11 reasoned non-producer exclusions; both callers now use PERF-WATCH:PRODUCER?. Completeness ratchet walks lib/ptx + src/arch/ptx + tools/ptx *-cg.f and fails on any unclassified producer - proven immediately at merge: it red-flagged the cuda-scope module (landed after the lane's base) until classified, exactly the mechanical ownership check the dot demanded. Red-first per acceptance item: each newly-watched producer touched without a row fails; near-miss/test paths stay quiet; every path resolves, duplicates throw; injected new producer trips the ratchet. Full gate at the merged tip: zero red phases, stable sha; wall-clock band deferred to the quiet-box re-verify (XLA build loading the box). Loose ends: tools/ptx drivers not ending -cg.f would escape the manifest arm (retarget trivially if a structured manifest lands); assembler-flag perf gating out of scope"
---

Full context: tools/kernel-perf-lint-core.f watches selected codegen paths but omits lib/ptx/tile*.f, tile-v4.f, opt.f, opt-ir.f, and ir.f even though they alter lowered PTX and GEMM performance. Fix: make the watch set a checked canonical table covering every PTX IR/tile/optimizer producer path, with exact path matching and a focused completeness ratchet against the owning manifest; do not watch tests/docs or use broad directory prefixes. Acceptance: touching each newly named producer without a profile/waiver row fails; near-miss/test paths do not trigger; every watched path resolves and duplicates reject; a manifest producer addition without watch ownership fails. Files: tools/kernel-perf-lint-core.f, tools/kernel-perf-lint-test.f, tools/ptx/perf-registry.f or a focused watch-table module if factoring keeps one concern per file, FILEMAP.md if new. Verify: focused positive/negative matrix, stdlib PTX lint slice, host/filemap/trust/dot gates.

Claim: agent=kperf workspace=.jj-ws/fable-kperf machine=spark (owns tools/kernel-perf-lint-core.f/test + watch-table factoring + FILEMAP)
