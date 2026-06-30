---
title: PTX IR value fixtures
status: closed
priority: 2
issue-type: task
created-at: "2026-06-30T08:43:17.711982+02:00"
closed-at: "2026-06-30T08:53:26.424937+02:00"
close-reason: "completed locally: checked PTX IR value layer plus fold/peephole/CSE/DCE fixtures; focused PTX suite, stdlib error fixture, lints, cold and hot local full suite green; zed untouched"
---

Problem: habu-ptx-ir-opt is too broad to land safely in one change. First local-only slice: add a checked PTX expression IR value layer plus fold/DCE/peephole fixtures that run on macOS without ptxas/CUDA. Files: lib/ptx/ir.f, lib/ptx/ir-test.f, FILEMAP.md, test/gate-stdlib-cases.f or the owning PTX static suite. Verify: focused PTX IR tests pass, typed-local-diff-lint pass, filemap/host/dot lints pass, full local native suite hot green. Zed/device validation out of scope.

2026-06-30 local proof: added `lib/ptx/ir.f` as a checked value-numbered expression IR using structure records, construction-time constant fold/peephole/CSE, and a root live-mark DCE count. Added `lib/ptx/ir-test.f` to the local `ptx-stdlib` suite and PTX IR error constants. Focused PTX static suite passed; `stdlib-errors-test` passed; typed-local-diff-lint, dot-dep-lint, stale-status-lint, host-lint, and filemap-lint passed; full local native suite passed cold 41671ms internal / 43.802s wall and hot 25003ms internal / 27.222s wall. Zed/device validation intentionally untouched.
