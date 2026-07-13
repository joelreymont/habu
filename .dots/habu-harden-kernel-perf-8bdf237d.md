---
title: "Harden kernel-perf-lint: hunk-aware diff, waiver ratchet, IR watch-set"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T18:16:23.861730+02:00"
---

Problem (kbench destruction-review advisories, tools/kernel-perf-lint-core.f): (a) an added line beginning '++ b/<file>' spoofs a diff file-header (same no-@@-tracking model as typed-local-diff-lint-core.f:170-171), crediting rows without touching the TSV — track @@ hunks and add a negative fixture in both lints; (b) a WAIVER row has no ratchet or stale-waiver lint and need not match the touched emitter, so a kernel can escape perf gating forever behind a fresh waiver — add a stale-waiver finding (after N days or on next touch of the same kernel) and bind the waiver kernel to the changed emitter; (c) the watch-set misses lib/ptx/tile*.f, tile-v4.f, opt.f, opt-ir.f, ir.f which lower kernel IR and change emitted PTX — extend the watch-set (most GEMM-ladder wins live there). Acceptance: fixtures for each. Files: tools/kernel-perf-lint-core.f + tests, tools/typed-local-diff-lint-core.f (hunk tracking). Verify: kernel-perf-lint-test.f, typed-local-diff-lint on a spoof fixture. Depends: none. Ownership: kernel-perf-lint. Claim: unassigned.
