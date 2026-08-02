---
title: Delete Maki CUDA shims
status: closed
priority: 1
issue-type: task
created-at: "2026-07-29T23:22:19.484052+02:00"
closed-at: "2026-08-02T22:06:59.264045+02:00"
close-reason: "Landed on origin/master at 6df493c76fcd: Maki CUDA shims deleted and all callers use canonical CUDA owner; full test/run.f, Maki/device, PTX/lint-libs, and exact ownership gates green; independent review ACCEPT."
---

Why: `maki/cuda-types.f` and `maki/cuda-driver.f` only forward the canonical
`lib/ptx/cuda-driver.f`; `E-MK-GPU`, global `CUDA-HANDLE0`/`CUDA-RC0`, and the
nineteen unhyphenated `CUDA:` C-spelling methods duplicate canonical names.
Result: migrate every live caller and fixture to `lib/ptx/cuda-driver.f`,
`E-CUDA`, `CUDA:HANDLE0`/`CUDA:RC0`, and the hyphenated `CUDA:CU-*` methods;
implement the two package guards directly; delete both Maki wrappers, their
wrapper-only tests, the two globals, all nineteen forwarding methods, and the
duplicate `cuFuncSetBlockShape` symbol lookup. Update only real load, inventory,
and current documentation edges. Owner: the duplicate CUDA surface and its
direct callers. Acceptance: zero operational legacy reference remains;
canonical CUDA, Maki device, lower-launch, evaluator, typed/package diff, and
native gates pass. Forbidden: forwarding file, removed-name assertion,
tombstone, lint, ledger, alias, shim, deprecation period, version check,
compatibility name, fallback, or unrelated CUDA refactor. Claim:
agent=cuda_hard_cut workspace=.jj-ws/habu-delete-maki-cuda
