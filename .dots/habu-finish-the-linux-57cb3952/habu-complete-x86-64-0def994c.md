---
title: Complete x86-64 compiler emission
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.362761+03:00"
blocks:
  - habu-render-x86-idiv-9c6d9516
  - habu-render-x86-cmpsel-279135be
  - habu-render-x86-trap-33eba82b
  - habu-record-symbolic-x86-10037f07
  - habu-declare-and-select-bfbb301b
  - habu-emit-and-exec-a8536cf2
---

Lane C: complete `X64EMIT`. Render the twelve refused forms (`emit-x64.f:40-51`: `neg`, `shl`, `shr`, `idiv`, `cmpsel`, `selz`, `reserve`, `release`, `store`, `load`, `trap`, `codeaddr`) and scalar floats, record every call, tail-call, `codeaddr` and `MOVABS` site as a row, and make spans exact. Spark builds and runs the focused suites; the ThinkPad executes the routine images. C8 first, because every other C leaf verifies through it. Files: `src/compiler/native/{emit-x64,select-x64,x64ir}.f`, `test/compiler/x64-*`, `test/x86-64-peer-routines.f`.
Leaves: habu-run-emitted-x86-b704f918 (C8), habu-emit-x86-frame-d8d25223 (C1), habu-render-x86-neg-43e4e8f8 (C2), habu-render-x86-idiv-9c6d9516 (C3), habu-render-x86-cmpsel-279135be (C4), habu-render-x86-trap-33eba82b (C5), habu-record-symbolic-x86-10037f07 (C6), habu-declare-and-select-bfbb301b (C7a), habu-emit-and-exec-a8536cf2 (C7b).
Campaign lane only; do not dispatch. It lists its leaves under blocks: so it stays off dot ready until they close.
Ownership: krait (Intel lane).
Claim: unassigned.
