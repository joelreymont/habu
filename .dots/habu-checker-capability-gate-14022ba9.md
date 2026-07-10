---
title: "Checker capability: patch32 gated TRUSTED-only (E-CAP-TRUSTED)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-10T00:00:00+02:00"
---

Checker capability: patch32 gated as TRUSTED-only (E-CAP-TRUSTED).

RCA: a CHECKED program could call patch32 (plain PRIM effect in checker.f) to
inject raw machine code and forge the ctor/friend seal (the F3 forge); the fix
marks patch32 PRIM-TRUSTED-ONLY so checked code is rejected, and confines it to
thin audited TRUSTED: wrappers.

Owner-of-record for the patch32 code-emission boundaries: FFI-PATCH
(lib/ffi-abi.f), TASK-PATCH (lib/task.f), ES-PATCH32 (test/engine-suite.f). The
ES-PATCH32 test-metaprog row stays folded in engine-suite.f's file-level count
for now (buried in its 51 test-metaprog file-level rows, not cleanly separable
into a `file:name` row) — separate cleanup. Landed on fable; branch fable.
