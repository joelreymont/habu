---
title: Rest FFI declaration scratch at file boundaries
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T00:00:20.052614+02:00"
closed-at: "2026-10-29T12:23:03.084877+02:00"
close-reason: "Landed acc2887c: FFI-DECL rests its scratch on every exit (KEEP-REST); replay e2e FFI-REUSE per file; full suite 496/496 (lsp-boundary timeout under load, rerun ok)"
---

Problem: a cached file that declares FFI functions, reused warm, leaves a product that differs from a cold build's (41 bytes on /tmp/carl-l6/x/d, master's builder): FFI-ABI declaration buffers (src/lib/ffi-abi.f NAME-BUF SYM-BUF TOK-BUF BASE-BUF and -U cells) and CODEGEN buffers (LIB-NAME GEN EFF DIAG; control record in DATA) keep the last declaration's bytes and the cache capture keeps them. Found by lane L6 (its FFI-LOAD-CASE fails SAME-FILE on it). Acceptance: per-declaration FFI cells inside CHECKER-REG:SCRATCH{ spans; CODEGEN buffers rest at a file boundary as at image capture; new replay e2e FFI-REUSE-CASE fails on master and passes; full suite. Brief: carl-ffiscr/.out/brief-ffiscr.md. Claim: carl, workspace .jj-ws.noindex/carl-ffiscr (worker running).

carl 2026-10-29: the checker scratch span is dropped. CHECKER-REG:SCRATCH{ is DNAME-INT after the seal, so ffi-abi.f cannot run it, and a span would link ffi-abi.f DATA into checker.f by distance, breaking file-unit reuse. Instead CLOSE-FUNCTION and REFUSE rest the per-declaration cells at zero; DECL-CLOSED-CK already refuses caching a segment that closes inside a declaration, so the cells are zero at every cacheable boundary. The GEN rule is reverted with it. Brief: carl-ffiscr/.out/brief-ffiscr3.md.

Review of onnsrluu: REJECT. (A) A declaration that throws inside a catch in a word's body can leave dirty cells in a reused file. The root cause is the cache: an unjudged run's after closes no ticket (lane carl-abandon, dot habu-refuse-runs-abandoned, its sibling). (B) VERSIONED-LIBRARY leaves TOK/BASE set: fold-in. Fold-in brief carl-ffiscr/.out/brief-ffiscr4.md (rebase, B, drop REST from REFUSE, comment, VERSIONED-LIBRARY replay case). Lands after carl-abandon.
