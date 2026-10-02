---
title: Bind x86 host ABI and select backend by target
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.612606+03:00"
closed-at: "2026-09-30T18:30:45+03:00"
close-reason: NABI:BINDING reads TARGET-ARCH and compiler.f LOAD-PASSES by target; lint 101 files 0 findings, chain fixpoint e2467d40 from gen1, gate 511/511 green, x86 proof unchanged.
---

Problem: `src/compiler/native/abi.f:44-52` hard-codes AArch64; `src/compiler/native/compiler.f:37` requires `src/arch/arm64/passes.f` unconditionally; the manifest carries no backend by target.
Acceptance: `src/arch/x86-64/abi.f` publishes the sysv-amd64 binding; `NABI:BINDING` answers it on an x86 host; `compiler.f` requires the passes module by target; `src/habu/native-runtime.f` carries `x64ir.f`/ `select-x64.f`/`emit-x64.f`/`passes.f` on x86 and the arm64 set otherwise; `tools/manifest-lint.f` green on both.
Files: `src/arch/x86-64/abi.f`, `src/compiler/native/abi.f`, `src/compiler/native/compiler.f`, `src/habu/native-runtime.f`.
Verify: spark: focused suites, `tools/manifest-lint.f`; rebuild; gate.
Depends: habu-fill-nemit-from-d8c030e4 (P3). Serialise with X1 on `compiler.f`.
Route: Alder (shared: src/compiler/native/abi.f, src/compiler/native/compiler.f, src/habu/native-runtime.f).
Ownership: krait (Intel lane).
Claim: unassigned.

Preflight corrections (2026-09-30; override the lines above where they differ):
- P3 is closed; X1 waits on this dot, so drop the serialise line. Route: krait lands on master after the spark proof. `X64ABI:BINDING` already exists (`src/arch/x86-64/abi.f:60-66`).
- "x86 host" means `HB-TARGET-LINUX-X86-64?` (`src/os/*/target.f:9`). `NABI:BINDING` reads it when called, so it reports the running engine. The passes choice reads it when `compiler.f` loads; in a build window that is the window's `target.f` (`tools/native-build-core.f:137-156`). `X64LAYOUT` is a layout replay, not a switch.
- `src/compiler/native/abi.f`: add a private `TARGET-ARCH`, twin of `TARGET-ABI` (30-34): AARCH64 on linux and macos, X86-64 on linux-x86-64, otherwise `E-CTGT-ABI`. Change `BINDING` (45-51) to `TARGET-ARCH TARGET-ABI …` and leave the other fields alone, so on x86 it is `CBIND:SAME?` as `X64ABI:BINDING`. Do not require the x86 `abi.f`, which would put `x64ir.f` in the ARM64 image. Rewrite the comments at 36-44 and x86 `abi.f:54-59`.
- `compiler.f:37` becomes a private `LOAD-PASSES`, shaped like `src/habu/aot-lib.f:16-27`. Put each `s" …" required` on its own line (as `native-runtime.f:112-129` does) so `tools/manifest-lint-core.f:134-143` reads both edges. Update the comment at 17-23.
- Files: remove `src/habu/native-runtime.f`, because its row :109 carries whatever `compiler.f` loads. Add `docs/x86-64.md:1469-1470`, which goes stale.
- Baked files: `abi.f` and `compiler.f`. The ARM64 product will not stay byte-identical, because the new words are captured. It must converge instead.
- Out of scope: loading the x86 closure. `src/arch/arm64/machine.f:71-81` throws `E-CTGT-ABI` at load on non-aarch64, and both `compiler.f:30` and x86 `passes.f:50` reach it. `habu-select-the-build-610b4492` meets it first, and its "backend rows" means `LOAD-PASSES`.
- Pre-change check: nothing fails on an ARM64 engine. On master, NABI's x86 contract throws -6610, and the lint reports 88 files in the closure with 0 findings.
- Tests: none, and no booted image. No image runs the compiler, so the x86 arms can only be inspected until X6.
- Verify on spark: the lint is target-independent, so one run covers both arms and must report 101 files and 0 findings. Then run suites compiler-native-chain, compiler-x64-chain and manifest-lint-fixtures; rebuild; confirm `X64PASS:INSTALL` still gives E-UNDEFINED; run `tools/two-generation-build.f` to convergence; run the gate on hb-b5.
- Overlap: `docs/x86-64.md` only, different sections.
