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
  - habu-emit-x86-syscall-a0d501db
  - habu-emit-x86-control-9a35e3b3
  - habu-emit-x86-atomics-c02e092e
  - habu-emit-x86-engine-86b5f8e7
  - habu-emit-x86-float-38de4a6f
---

Problem: no x86 image yet carries a captured Habu program; this is the executable milestone M3, reached before the interpreter lands.
Acceptance: a cross-build whose window loads the prefix plus `test/prim-parity.f` as the entry produces `hb-x64-parity`; on the ThinkPad it prints the parity report and exits 0; the negative twin (one wrong expectation) exits nonzero.
Files: `tools/native-build.f` (entry selection), a test driver under `test/x86-64-*`.
Verify: spark cross-builds both images; ThinkPad `./hb-x64-parity; echo $?` prints the report and 0, the negative twin nonzero.
Depends: habu-select-the-build-610b4492 (X3), habu-resolve-x86-entry-cb671d4d (X4d), habu-emit-x86-pure-f70fb84b (K5), habu-emit-x86-syscall-a0d501db (K6), habu-emit-x86-control-9a35e3b3 (K7), habu-emit-x86-atomics-c02e092e (K8), habu-emit-x86-engine-86b5f8e7 (K9), habu-emit-x86-float-38de4a6f (K13).
Route: Alder (shared: tools/native-build.f).
Ownership: krait (Intel lane).
Claim: unassigned.
