---
title: Add CPU threads + atomics + re-entrancy to Habu engine
status: open
priority: 1
issue-type: task
created-at: "2026-06-28T12:23:07.811874+02:00"
---

Habu is single-threaded; the Odin perception port needs real OS threads (4x60Hz acquisition + parallel detector lanes) over the thin C SDK shim. DESIGN: docs/threads.md (settled; codex oracle GO-WITH-CHANGES, 3 passes). Adopts the SwiftForth task interface (TASK/ACTIVATE/HALT/KILL/PAUSE + user variables +USER/#USER/HIS, user-area base in a register) backed by pthread. Scope: (0) audit every engine DATA cell as compile-only (stays global) vs runtime-touched (-> user variable) — codex enumerated: runtime = S0/RSP/LOOPSP/LVD/VSP/HND/INP/INE/EVALD/TKA/TKL/REPLH + FFI scratch (ffi.f:9); compile-only = DP/CP/NDICT/TSIG/QPATCH/LOCN/BODYLEN/EXITH/PEND/DOESB/TRUSTED/VRFREE. (1) user-variable layer + migrate runtime cells (riskiest step; needs a multi-task aliasing fixture since single-thread gates miss cross-thread corruption). (2) pthread trampoline + TASK/CONSTRUCT/ACTIVATE/HALT/KILL. (3) atomics ATOMIC@/!/ADD/CAS + FENCE (LDAR/STLR/LDADD/CAS/DMB ISH). (4) GET/RELEASE over pthread mutex. REQUIRED INVARIANTS (fail-closed): user-area base register authoritative across JIT/FFI/trampoline/throw-catch/eval (+ diagnostic for stale DATA-relative refs); 'no compilation while tasks live' enforced at the compiler/runtime boundary. exit via exit_group(94) not exit(93) — fixes the CUDA-thread lingering RCA. lib/memory.f needs no change (mmap-per-call). Fixpoint stays single-threaded; positive + negative checker/gate fixtures; full gate green. Blocks: Habu perception-orchestration port (Odin-port-live-capture-2b2d345e).
