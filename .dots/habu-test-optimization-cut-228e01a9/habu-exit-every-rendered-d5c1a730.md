---
title: Exit every rendered refusal as a refusal
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T12:43:43.185096+02:00"
---

After 5f2a218e (1b43601c) tools/check.f exits 70 for a run ending on E-TRUST-UNRESOLVED or E-PKG-CONTEXT, keyed on the engine's last stderr line. Measured (review 430): evaluated E-BAD-DECLARATION (E-TDECL-*, e.g. 7107) and evaluated E-USING-SHADOW-GLOBAL (7141) write a 'verdict':'rejected' record with a repair class and still exit 67 through check.f, the same status as CHK-E-CAPACITY (tools/check-core.f:72, 'source path exceeds capacity'), so a repair loop keyed on the exit code cannot tell them from an overlong path; --load exits 67 for every rendered refusal although src/core/checker.f ~1202 says 67 is not for a compile-time refusal. The structural layer is the engine's top-level reporter (src/habu/habu2.f LUNCRPT): exit the refusal status for a throw whose diagnostic was rendered, keeping the catchable code, and drop check.f's per-code CHK-RUN-STATUS list. Acceptance: every run-stage refusal that writes a record exits a refusal status through --load and check.f, a bare program throw keeps UNCAUGHT-RC, CHK-E-CAPACITY no longer shares a status with either, tests asserting --load 67 re-pinned with reasons; engine rebuild with multi-generation convergence.
