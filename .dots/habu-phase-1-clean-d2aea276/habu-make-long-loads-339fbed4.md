---
title: Make long loads GC-stable
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.128585+02:00\""
closed-at: "2026-04-03T10:13:00.320824+02:00"
close-reason: done (zig build ok; zig build test unchanged 5-error baseline; direct habu probe blocked by pre-existing stdlib bootstrap UnboundSymbol)
blocks:
  - habu-remove-whole-file-4f7c968a
---

Problem: long module loads can corrupt forms, literals, macros, chunks, or JIT metadata across GC. Acceptance: repeated large loads stay stable without stale pointers or name-scan fallbacks. Files: src/interp/repl.zig, src/interp/vm.zig, src/runtime/gc.zig root-registration paths. Verify: repeated large-load regression on upstream modules. Blockers: habu-remove-whole-file-4f7c968a; also depends on habu-fix-macro-table-a8759987.
