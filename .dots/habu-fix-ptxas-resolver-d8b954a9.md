---
title: Fix ptxas resolver test contract for CUDA-less hosts
status: active
priority: 1
issue-type: task
created-at: "2026-07-19T00:52:29.000000+02:00"
---

STOP-THE-LINE incident 2026-07-19: master@origin f32be516 is red on any host without a CUDA toolchain - bin/hb --load maki/test.f fails at maki/device-artifacts-test.f (rc -3424 E-PTXTC-PTXAS) and test/run.f fails at lib/ptx/toolchain-test.f + the stdlib/lint-libs/ptx-toolchain group. Root cause: the old PTXTC:PTXAS$ silently returned a hardcoded path without checking existence (forbidden silent fallback); commit 59971de0 correctly made it fail closed (env override, then /usr/local/cuda/bin/ptxas, then legacy 12.6, else E-PTXTC-PTXAS throw), but both tests keep the unconditional assertion 'PTXAS$ nip 0 > TTRUE', which now requires ptxas to exist on every test host. The Mac gate host has no CUDA, so the owning suites red while the DGX host stays green. Fix (no skip hacks): add an option-typed probe TRY-PTXAS$ ( -- option<path> ) to package PTXTC probing env + the two install paths with FILE?, reimplement PTXAS$ as the fail-closed layer over it (MATCH present -> path, absent -> E-PTXTC-PTXAS throw; no path-list duplication), and rewrite the assertions in lib/ptx/toolchain-test.f PREPARE-PATHS and maki/device-artifacts-test.f PREPARE-PATHS to assert the contract per host class: probe present -> PTXAS$ returns that non-empty path; probe absent -> PTXAS$ throws E-PTXTC-PTXAS via TTHROWSQ. Both branches are real assertions, so the suite is green and meaningful on both host classes. Register TRY-PTXAS$ where the toolchain public surface is documented. Gates: lib/ptx/toolchain-test.f, maki/device-artifacts-test.f, full test/run.f + maki/test.f on this Mac, host-lint, filemap-lint.

Claim: agent=fix-ptxas workspace=.jj-ws/habu-fix-ptxas-resolver-d8b954a9
