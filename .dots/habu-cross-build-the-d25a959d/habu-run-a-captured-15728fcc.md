---
title: Run a captured Habu program on x86-64 (M3)
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.740651+03:00"
blocks:
  - habu-select-the-build-610b4492
  - habu-resolve-x86-entry-cb671d4d
  - habu-emit-x86-pure-f70fb84b
  - habu-emit-x86-engine-86b5f8e7
  - habu-emit-x86-float-38de4a6f
  - habu-emit-x86-code-973a0074
  - habu-model-the-code-c40c75d1
  - habu-add-sysv-abi-75f86980
  - habu-add-the-x86-efc81b26
  - habu-port-the-profiler-97103e6e
---

Problem: no x86 image yet carries a captured Habu program; this is the executable milestone M3, reached before the interpreter lands.
Acceptance: a cross-build whose window loads the prefix plus `test/prim-parity.f` as the entry produces `hb-x64-parity`; on the ThinkPad it prints the parity report and exits 0; the negative twin (one wrong expectation) exits nonzero.
Files: `tools/native-build.f` (entry selection), a test driver under `test/x86-64-*`.
Verify: spark cross-builds both images; ThinkPad `./hb-x64-parity; echo $?` prints the report and 0, the negative twin nonzero.
Depends: habu-select-the-build-610b4492 (X3), habu-resolve-x86-entry-cb671d4d (X4d), habu-emit-x86-pure-f70fb84b (K5), habu-emit-x86-syscall-a0d501db (K6), habu-emit-x86-control-9a35e3b3 (K7), habu-emit-x86-atomics-c02e092e (K8), habu-emit-x86-engine-86b5f8e7 (K9), habu-emit-x86-float-38de4a6f (K13).
Route: Alder (shared: tools/native-build.f).
Ownership: krait (Intel lane).
Claim: unassigned.

K-lane corrections (design 2026-09-30): Depends add K6b (habu-emit-x86-process-8e1f6f84), K8b (habu-emit-x86-code-973a0074), K9b (habu-model-the-code-c40c75d1), K9c (habu-emit-x86-heap-0b1d01f0), K9d (habu-emit-the-x86-f74e1d26). `ENGINE-PRIMS:COMPLETE` refuses every kept row without a body, so the first x86 kernel build also needs the `ffi-*` (K11a, K11b), `task-entry` (K11c) and `prof-*` (K10c) bodies: Depends add those four. Refusal rows would be stubs the later leaves delete; the real bodies are the long-term shape.
