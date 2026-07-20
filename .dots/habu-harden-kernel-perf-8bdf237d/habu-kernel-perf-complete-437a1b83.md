---
title: "Kernel perf: complete IR watch set"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-15T07:30:08.222046+02:00\""
blocks:
  - habu-kernel-perf-ratchet-7d99cab2
---

Full context: tools/kernel-perf-lint-core.f watches selected codegen paths but omits lib/ptx/tile*.f, tile-v4.f, opt.f, opt-ir.f, and ir.f even though they alter lowered PTX and GEMM performance. Fix: make the watch set a checked canonical table covering every PTX IR/tile/optimizer producer path, with exact path matching and a focused completeness ratchet against the owning manifest; do not watch tests/docs or use broad directory prefixes. Acceptance: touching each newly named producer without a profile/waiver row fails; near-miss/test paths do not trigger; every watched path resolves and duplicates reject; a manifest producer addition without watch ownership fails. Files: tools/kernel-perf-lint-core.f, tools/kernel-perf-lint-test.f, tools/ptx/perf-registry.f or a focused watch-table module if factoring keeps one concern per file, FILEMAP.md if new. Verify: focused positive/negative matrix, stdlib PTX lint slice, host/filemap/trust/dot gates.

Claim: agent=kperf workspace=.jj-ws/fable-kperf machine=spark (owns tools/kernel-perf-lint-core.f/test + watch-table factoring + FILEMAP)
