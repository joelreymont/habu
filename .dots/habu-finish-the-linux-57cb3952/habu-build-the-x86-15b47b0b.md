---
title: Build the x86-64 kernel
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.380560+03:00"
blocks:
  - habu-emit-x86-pure-f70fb84b
  - habu-port-the-crash-99c87339
  - habu-port-the-profiler-97103e6e
  - habu-add-sysv-abi-75f86980
  - habu-add-the-x86-efc81b26
  - habu-bind-x86-host-4485acdd
  - habu-emit-x86-float-38de4a6f
  - habu-port-the-profiler-c77ee1af
  - habu-share-the-parity-045ddf20
  - habu-emit-the-x86-e49a3447
---

Lane K: the small hand-written x86-64 kernel bound by `src/habu/prims.f` names: the code layer (`src/arch/x86-64/icode.f`), runtime moves and the per-target `DSTACK` (`rt.f`, `layout.f ENGINE-GPR`), bodies in `src/habu/kernel-x64.f`, boot in `src/habu/boot-x64.f` (`_start`, stacks, signals, FFI trampolines, task entry), the registry and completeness gate shared out of `habu1.f`/`habu2.f`, float primitive bodies, and the x86 host binding with backend selection by target. The kernel keeps `_start`, VM registers, runtime stacks with guards, the name-index rebuild (`seed-ndict!`), argc/argv/envp/heap-floor cells, signal-handler install and the call through `ENGINE-MAIN:XT-CELL`; it does no seed relocation (the writer links at write time). Built on spark, run on the ThinkPad. Serialise: `habu2.f` (K4, I6, I10c, X7); `layout.f` (K2, P2); `tools/native-emit.f` (K3, X4d); `compiler.f` (K12, X1).
Leaves: habu-add-the-x86-aad02c7e (K1), habu-add-the-x86-a8bf9973 (K2), habu-boot-and-exit-367c46f5 (K3), habu-share-the-primitive-58c235e5 (K4), habu-emit-x86-pure-f70fb84b (K5), habu-emit-x86-syscall-a0d501db (K6), habu-emit-x86-control-9a35e3b3 (K7), habu-emit-x86-atomics-c02e092e (K8), habu-emit-x86-engine-86b5f8e7 (K9), habu-port-signals-crash-2c7768ca (K10a), habu-port-the-crash-99c87339 (K10b), habu-port-the-profiler-97103e6e (K10c), habu-add-sysv-ffi-17a130a1 (K11a), habu-add-sysv-abi-75f86980 (K11b), habu-add-the-x86-efc81b26 (K11c), habu-bind-x86-host-4485acdd (K12), habu-emit-x86-float-38de4a6f (K13).
Campaign lane only; do not dispatch. It lists its leaves under blocks: so it stays off dot ready until they close.
Ownership: krait (Intel lane).
Claim: unassigned.
