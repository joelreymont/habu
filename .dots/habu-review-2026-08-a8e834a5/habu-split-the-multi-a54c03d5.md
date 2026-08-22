---
title: split the multi-concern test files
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:47:07.082224+02:00"
---

Problem: test/engine-suite.f (2801 lines, one stdin program) covers twelve concerns (arithmetic, checker candidates, quotations, registry whitebox, record layouts, FFI, raw syscalls, floats, JIT pressure, linear types, value records, PTX families, multi-error) with a shared F<n> counter; native-elaborate.f 3244, native-regalloc.f 3137, native-select.f 2883, gate-engine-lib.f 2586, type-decl-suite.f 2517, native-hir.f 2288, native-migrate.f 2128 each registered as one SUITE, so a red names the file, not the concern. Acceptance: split per concern (docs/forth.md one concern per file), each registered; a red names the concern. Files: as listed. Verify: gate green with the same assertion count. Depends: the registration-table dot. Ownership: tests. Claim: unassigned.
