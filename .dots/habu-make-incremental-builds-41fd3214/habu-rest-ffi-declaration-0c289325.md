---
title: Rest FFI declaration scratch at file boundaries
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T00:00:20.052614+02:00"
---

Problem: a cached file that declares FFI functions, reused warm, leaves a product that differs from a cold build's (41 bytes on /tmp/carl-l6/x/d, master's builder): FFI-ABI declaration buffers (src/lib/ffi-abi.f NAME-BUF SYM-BUF TOK-BUF BASE-BUF and -U cells) and CODEGEN buffers (LIB-NAME GEN EFF DIAG; control record in DATA) keep the last declaration's bytes and the cache capture keeps them. Found by lane L6 (its FFI-LOAD-CASE fails SAME-FILE on it). Acceptance: per-declaration FFI cells inside CHECKER-REG:SCRATCH{ spans; CODEGEN buffers rest at a file boundary as at image capture; new replay e2e FFI-REUSE-CASE fails on master and passes; full suite. Brief: carl-ffiscr/.out/brief-ffiscr.md. Claim: carl, workspace .jj-ws.noindex/carl-ffiscr (worker running).
