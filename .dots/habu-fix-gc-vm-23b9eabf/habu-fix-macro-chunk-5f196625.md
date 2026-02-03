---
title: Fix macro chunk pool
status: open
priority: 2
issue-type: task
created-at: "2026-02-03T10:40:25.678148+01:00"
blocks:
  - habu-fix-repl-chunk-dd041c71
---

src/compiler/compile.zig:2622-2665 + 6130+: macro expansion sets vm.chunk_pool to temp slice freed on return; also compiled chunks not rooted for GC during macroexpansion. Fix: run macroexpansion inside saved VM state that restores chunk_pool/global_env; root emitted chunk Values via vm.ext_roots while running; add regression macro that allocs enough to GC. Verification: zig build test.
