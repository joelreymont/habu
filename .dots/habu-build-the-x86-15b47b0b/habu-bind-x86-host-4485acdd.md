---
title: Bind x86 host ABI and select backend by target
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.612606+03:00"
blocks:
  - habu-fill-nemit-from-d8c030e4
---

Problem: `src/compiler/native/abi.f:44-52` hard-codes AArch64; `src/compiler/native/compiler.f:37` requires `src/arch/arm64/passes.f` unconditionally; the manifest carries no backend by target.
Acceptance: `src/arch/x86-64/abi.f` publishes the sysv-amd64 binding; `NABI:BINDING` answers it on an x86 host; `compiler.f` requires the passes module by target; `src/habu/native-runtime.f` carries `x64ir.f`/ `select-x64.f`/`emit-x64.f`/`passes.f` on x86 and the arm64 set otherwise; `tools/manifest-lint.f` green on both.
Files: `src/arch/x86-64/abi.f`, `src/compiler/native/abi.f`, `src/compiler/native/compiler.f`, `src/habu/native-runtime.f`.
Verify: spark: focused suites, `tools/manifest-lint.f`; rebuild; gate.
Depends: habu-fill-nemit-from-d8c030e4 (P3). Serialise with X1 on `compiler.f`.
Route: Alder (shared: src/compiler/native/abi.f, src/compiler/native/compiler.f, src/habu/native-runtime.f).
Ownership: krait (Intel lane).
Claim: unassigned.
