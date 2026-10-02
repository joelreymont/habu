---
title: Run build-fixpoint on an x86 host
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T19:36:44.712472+03:00"
blocks:
  - habu-run-bin-hb-6378f297
  - habu-resolve-x86-entry-cb671d4d
---

Problem: self-hosting on the ThinkPad runs the chain through `tools/build-fixpoint.f`, whose x86 branches cannot load as ordered. They run only on an x86 host (no `--target`; `BUILD-TARGET` is the host). `BF-APPEND-COMMON` appends the ARM64 asm and icode layer (globals `CODE`, `ASM-LEN`, `CODE-CAP-BYTES`) ahead of the seam, so the bare `X64CODE` tails in `elf.f` and `image-bytes.f` are refused whatever modules are added. X4d measured this and left build-fixpoint unchanged.
Acceptance: on the ThinkPad, with the x86 `hb` as host, build-fixpoint loads the x86 code layer and no ARM64 code globals. That layer is `asm.f`, `icode.f`, `rt.f` and `image-bytes.f`, loaded as modules ahead of the seam the way X4d's `NATIVE-EMIT` route loads them. Gen N+1 builds from gen N. ARM64 hosts are unchanged: spark gen2 == gen3.
Files: `tools/build-fixpoint.f`.
Verify: a ThinkPad build-fixpoint run with the x86 host; spark chain.
Depends: habu-run-bin-hb-6378f297 (M4), habu-resolve-x86-entry-cb671d4d (X4d).
Ownership: krait (Intel lane).
Claim: unassigned.
