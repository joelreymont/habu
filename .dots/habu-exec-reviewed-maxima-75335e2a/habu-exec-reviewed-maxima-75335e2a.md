---
title: Execute reviewed Maxima+SBCL parity PLAN.md
status: open
priority: 1
issue-type: task
created-at: "2026-03-08T17:08:50.291311+01:00"
---

PLAN.md (2026-03-08 reviewed version): execute the reviewed plan in dependency order without disturbing existing WIP dots. Scope covers canonical Maxima execution, remaining correctness blockers, benchmark truthfulness, JIT/GC safety, and SBCL-parity work. Key files: PLAN.md, tools/maxima-rtest.lisp:1-83, lib/maxima-post-load.lisp:223-307, lib/maxima-loader.lisp:27-88, src/interp/repl.zig:2779-3020, src/testing/compile_chunk.zig:68-253, src/jit/backend.zig:2244-2444,2528-2635, bench/maxima_workload.zig, tools/maxima-bench. Overlaps existing WIP dots habu-finish-curr-maxima-51d5d460 / habu-optimize-hoist-for-39413c0a / habu-drive-habu-toward-4ff1134b; this root decomposes them into atomic executable children.
