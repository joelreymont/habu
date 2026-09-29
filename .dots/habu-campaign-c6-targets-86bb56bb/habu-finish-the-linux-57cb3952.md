---
title: Finish the Linux x86-64 backend
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.343961+03:00"
blocks:
  - habu-complete-x86-64-0def994c
  - habu-make-sealed-emission-17971865
  - habu-build-the-x86-15b47b0b
  - habu-move-the-interpreter-d35780fa
  - habu-cross-build-the-d25a959d
  - habu-port-the-ffi-676f745d
  - habu-gate-and-self-7afc5ff3
  - habu-rewrite-intel-md-b8294274
  - habu-point-restart-md-3e7996ca
---

Mission: a tier-1-only Linux x86-64 engine. A small hand-written kernel per target binds the `src/habu/prims.f` names; the outer interpreter, definers, packages, source loading and a `MAIN` entry move into checked Habu, are captured into both the ARM64 and the x86-64 product, and are entered through `ENGINE-MAIN:XT-CELL`, so there is no x86 twin of `habu2.f` or `jit.f`. The x86 engine is cross-built from spark by dual emission: one HIR per definition, lowered and emitted twice, arm64 into the live region and x86 into a shadow keyed by record that the capture carries; a host-side Habu linker writes the x86 image with fixed segments at write time (no seed relocation at boot or snapshot restore on x86). The cross-built engine then rebuilds itself on the ThinkPad to a byte fixpoint, the release artefact. Invariants: no cold route on x86 (`tools/native-build.f` is its only build route); every code-bearing record comes from the compiler; the capture reads recorded sites; the region and DATA are fixed `PT_LOAD`s on x86; `MAIN` is a DATA cell the kernel calls. The ARM64 product keeps tier 0 through the B1 hook (I6, chosen); tier-1-only products on both arches (B2) is a follow-on opened with G4b's numbers.
Hosts: spark (aarch64 Ubuntu 24.04, 20 cores, `~/Work/habu/krait`, product engine of master) builds, cross-builds and runs the ARM64 gate; the ThinkPad (x86-64 Arch, the lead) runs cross-built images until X6, then builds and gates natively. No CI. macOS is untested by us: Alder pulls master and fixes macOS, and every shared-file landing keeps macOS arms correct by construction and reports macOS untested. Terms the leaves use: gate = `bin/hb --load test/run.f` on spark from a tree whose `bin/hb` is the candidate (`docs/gate.md:8-19`); rebuild = `HABU_UNDER_TEST=$HOST HABU_FIXPOINT_ENGINE=$HOST HB_TMP=$TMP $HOST --load tools/native-build.f -- $OUT`; chain = `HB_TMP=$PWD/build/tmp bin/hb --load tools/two-generation-build.f -- <seed>`.
Landing routes: `Route: direct` when every file the leaf changes is x86-only (`src/arch/x86-64/`, `src/os/linux-x86-64/`, `src/compiler/native/{x64ir,select-x64,emit-x64}.f`, new x86-only files such as `src/habu/{kernel-x64,boot-x64,link-x64}.f`, `test/x86-64-*`, `test/compiler/x64-*`, `docs/x86-64.md`, `INTEL.md`, `.dots/`): the Intel lane lands it on master. `Route: Alder (shared: ...)` when the leaf changes a shared file (among them `test/gate-stdlib-cases.f`, `src/compiler/native/compiler.f`, `publish.f`, `native-runtime.f`, `layout.f`, `habu1.f`, `habu2.f`, `lib/*`, `tools/*`, `bootstrap/*`): it lands through Alder. Every leaf: one commit, one jj workspace under `.jj-ws/<id>`, gate then push on separate lines.
Structure: lanes habu-complete-x86-64-0def994c (C, compiler emission), habu-make-sealed-emission-17971865 (P, publication and sites), habu-build-the-x86-15b47b0b (K, kernel), habu-move-the-interpreter-d35780fa (I, interpreter in Habu), habu-cross-build-the-d25a959d (X, cross-build), habu-port-the-ffi-676f745d (R, runtime parity), habu-gate-and-self-7afc5ff3 (G, gate and self-host; G3 is habu-self-host-the-ccc31e78); the two documentation leaves habu-rewrite-intel-md-b8294274 (D1a, `INTEL.md`) and habu-point-restart-md-3e7996ca (D1b, `RESTART.md`) sit here. Documentation at each landing (design D3) is folded into the landing leaves' Files and Acceptance, with a closing pass in G3. Critical path: I1 -> I2/I3 -> I4 -> I5a-e -> I6 -> I7 -> I8 -> I9a-c -> I10a-c in parallel with P1 -> P2 -> P3 -> X1 -> X2a -> X2b -> X3 -> X4a-d -> X5 -> X6, then R2/R3 -> R4-R7 -> G1 -> G2 -> G3; X1 needs P3, K12 and C6; X5 needs K5-K9 and K13.
Serialised files (one open workspace at a time): `src/habu/habu2.f` (K4, I6, I10c, and X7 through the adjacent `habu1.f`); `src/habu/layout.f` (P2, K2); `src/compiler/native/publish.f` (P1, P3, X1); `src/compiler/native/compiler.f` (K12, X1); `src/arch/arm64/passes.f` and `src/arch/x86-64/passes.f` (P1, P3, X1); `src/habu/aot-capture.f` and `aot-closure.f` (X2a, X2b, I7); `src/habu/aot-decl.f` (X2b); `tools/native-emit.f` (K3, X4d). `src/compiler/native/hir-word.f` is not changed: callee identity is resolved through the capture's index. habu-make-build-fixpoint-eeaf6c00 is ARM64 recovery work (the install/stdin route; x86 has no cold route) and neither blocks nor is blocked by this campaign.
Every parent here lists its children under blocks: so only leaves show in dot ready. Layout (what `dot add -P` writes): a parent's own file sits in its parent's folder and its children in `.dots/<id>/`; `dot tree <id>` shows one level, and `dot fix` would flatten this layout, so do not run it.
Ownership: krait (Intel lane).
Claim: unassigned.
