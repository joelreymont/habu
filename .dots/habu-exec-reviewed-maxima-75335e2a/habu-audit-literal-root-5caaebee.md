---
title: Audit literal-root collector coverage
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-03-08T17:08:50.311277+01:00\\\"\""
closed-at: "2026-03-08T17:41:06.568017+01:00"
close-reason: "done: audited src/interp/repl.zig:2857-2984 and src/testing/compile_chunk.zig:125-252 against src/jit/backend.zig supported surface; explicit drift found (REPL missing list/list*, vec/str/hash/array families; compile_chunk missing list/list*/make_hash) and handed off to fix dots with focused validation evidence."
---

Files: src/interp/repl.zig:2779-3020, src/testing/compile_chunk.zig:68-253, src/jit/backend.zig:2244-2444,2528-2635. What: diff current supported JIT IR surface against both collectJitLiteralRoots implementations; enumerate live omissions (currently known: list/list_star plus REPL-vs-test divergence). Why: PLAN 8.0 requires closing present-day GC root omissions before any new JIT IR coverage work. Verification: produce explicit missing-tag list and choose shared/dedup fix strategy.
