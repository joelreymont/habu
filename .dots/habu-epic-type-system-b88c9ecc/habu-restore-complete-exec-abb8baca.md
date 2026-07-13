---
title: Restore complete execution frame across catch
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:19:33.087282+02:00"
---

Static invariant: a caught throw restores the complete caller execution frame: data SP, user return-stack depth, loop-stack depth, handler chain, machine SP, LR, and continuation PC. Problem: native and recovery BCATCH/BTHROW save and restore data and machine stack state but omit RSP-CELL and LOOPSP-CELL, while the checker models catch as restoring both typed stacks; a throwing quotation can therefore clobber caller return/loop state without a type mismatch. Fix: extend the native and bootstrap handler frame with both saved cells, restore them before resumption, and prove nested handler-chain integrity. Acceptance: a minimal checked r> after a throwing quotation preserves a caller-owned >r value; throw inside ?do restores loop state; nested catch and repeated throws do not drift either depth; underflow/forged-frame and adjacent-sentinel mutations fail before memory changes; native and recovery behavior agree. Files: src/habu/habu1.f, bootstrap/cg/forth.fs, new test/catch-frame.f, FILEMAP.md; docs/effects.md only if the existing catch contract needs clarification. Verify: red-first focused fixture, engine suite, bootstrap parity, clobber lint, typed-local diff, native fixpoint, host/filemap/dot lints, full native gate. Dispatch only after active habu1/bootstrap/layout/checker owners clear.
