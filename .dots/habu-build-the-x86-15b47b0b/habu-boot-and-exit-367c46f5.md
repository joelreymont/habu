---
title: Boot and exit the x86-64 kernel skeleton
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.534293+03:00"
blocks:
  - habu-add-the-x86-aad02c7e
  - habu-add-the-x86-a8bf9973
---

Problem: no x86 kernel exists and `tools/native-emit.f` cannot build one: its lines 2-4 and 44-61 require the arm64 assembler, `rt.f`, `crash.f`, `prof.f`, `regalloc.f`, `habu1.f`, `jit.f` and `habu2.f` unconditionally, and `src/habu/prof.f:47-50` dies on x86. Milestone M2.
Acceptance: `src/habu/boot-x64.f` emits `_start`: `rsp` from the kernel entry, `rbp` user area, `r12` data stack, `r13` DATA base, `r14` dictionary, `r15` code pointer (`docs/x86-64.md:47-57`), runtime stacks with guard pages (`STACK-ABI`), argc/argv/envp/heap-floor cells, exit 0; `tools/native-emit.f` gains a target-dispatched require set, so the x86 arm requires `src/arch/x86-64/{asm,icode,rt}.f`, the x86 seam, `kernel-x64.f`, `boot-x64.f` and `link-x64.f`; skeleton only at this leaf; `docs/x86-64.md` gains the kernel inventory section.
Files: `src/habu/boot-x64.f`, `tools/native-emit.f`, `docs/x86-64.md` (kernel inventory).
Verify: spark builds `hb-x64-skel` and the ARM64 product unchanged (rebuild); ThinkPad `./hb-x64-skel; echo $?` prints 0; `readelf -l hb-x64-skel`.
Depends: habu-add-the-x86-aad02c7e (K1), habu-add-the-x86-a8bf9973 (K2). Serialise with X4d on `tools/native-emit.f`.
Route: Alder (shared: tools/native-emit.f).
Ownership: krait (Intel lane).
Claim: unassigned.
