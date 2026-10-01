---
title: Select the build target in the capture window
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.723952+03:00"
closed-at: "2026-10-01T08:33:23+03:00"
close-reason: "native-build --target cell; spark x86 window opens linux-x86-64 target/layout/repl-term/passes, refuses without backend (entry test); host rebuild b4e05778 without machine.f, 2ef07f9f gen1=gen2 with it"
blocks:
  - habu-bind-x86-host-4485acdd
---

Problem: `src/habu/native-runtime.f:35-49 PROVIDE-TARGET`, `tools/native-build-core.f:159-175 NB-TARGET-CORE-FILES` and `tools/build-fixpoint.f:847-929` select the seam from the running engine.
Acceptance: `tools/native-build.f -- <out> [whitebox] [--target linux-x86-64]` sets a build-target cell read by those three and by the manifest's backend rows; the window loads the x86 `target.f`/`layout.f`/`repl-term.f`; the host's own predicates are untouched outside the window; refuses a target whose backend module is not loaded.
Files: `src/habu/native-runtime.f`, `tools/native-build-core.f`, `tools/build-fixpoint.f`, `tools/native-build.f`.
Verify: spark: a `--target linux-x86-64` window loads the x86 seam files and refuses without the x86 backend module; a host-target rebuild is unchanged (chain); gate.
Depends: habu-bind-x86-host-4485acdd (K12).
Route: Alder (shared: src/habu/native-runtime.f, tools/native-build-core.f, tools/build-fixpoint.f, tools/native-build.f).
Ownership: krait (Intel lane).
Claim: unassigned.

Lead note (2026-10-01, K12 landed): `NABI:BINDING` answers the host's arch through a private `TARGET-ARCH` (`src/compiler/native/abi.f`), and `compiler.f` loads the backend passes through a private `LOAD-PASSES` (arm64 on linux and macos, x86-64 on linux-x86-64); "the manifest's backend rows" means LOAD-PASSES. Loading the x86 closure still fails: `src/arch/arm64/machine.f:71-81` runs PLATFORM-RESERVED-MASK at load and throws `E-CTGT-ABI` on a non-aarch64 target, reached from `compiler.f:30` (`abi.f:19` -> `a64ir.f:36`) and from x86 `passes.f:50` (`frame.f:6` -> `a64ir.f`). This dot fixes that first. `test/compiler/native-chain.f:162-175` BINDING-CASE also throws `E-CTGT-ABI` on an x86 host.
