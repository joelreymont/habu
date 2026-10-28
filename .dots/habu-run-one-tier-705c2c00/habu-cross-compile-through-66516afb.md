---
title: Cross-compile through T0 dual emission
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:51.618180+02:00"
blocks:
  - habu-install-t0-as-6492133d
  - habu-write-the-t0-894e937c
---

Problem: the shadow/capture path needs the target arch's walk from the host's T0. Acceptance: T0:COMPILE walks host + publish, then when NSHADOW:OPEN? and not NATIVE? selects the shadow arch, walks again, NSHADOW:TAKE (TAKE-DOES); native-build-core.f loads the target emitter; native-emit-x64.f requires t0-x64.f. Files: src/habu/t0.f, tools/native-build-core.f, native-emit-x64.f. Verify: x86 engine cross-built by T0 on arm64; peer test/run.f; aot-shadow-capture; x86-64-link-records. Depends: L4, L5. Ownership: those files. Claim: unassigned.
