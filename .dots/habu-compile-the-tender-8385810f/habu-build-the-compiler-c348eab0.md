---
title: Build the compiler with the optimizer (selfbuild)
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-11T16:38:06.278925+03:00\""
---

Problem: the cold seed loads compiler.f before NCOMP:COMPILE exists, so the shipped compiler body is JIT code (IR-ID:COUNT-N opens with sub sp,sp,#16); every pass pays JIT frame-call overhead. Acceptance: a stage-2 engine whose compiler is compiled by the stage-1 optimizer, verified by the leaf disassembly probe and the span table reporting 0; the forced-tier Tender load measured on it as the next-generation baseline. Files: tools/native-build.f, src/habu/native-runtime.f, bootstrap. Verify: forced-tier Tender load on the stage-2 binary. Depends: none. Ownership: cedar (workspace .jj-ws/cedar-aot-selfbuild). Claim: agent=cedar
