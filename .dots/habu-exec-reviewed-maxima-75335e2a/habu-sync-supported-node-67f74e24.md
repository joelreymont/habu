---
title: Sync supported-node root coverage across REPL and compile_chunk
status: closed
priority: 1
issue-type: task
created-at: "\"2026-03-08T17:08:50.322112+01:00\""
closed-at: "2026-03-08T17:41:06.577512+01:00"
close-reason: "done: synced REPL collector coverage for current supported vec/str/hash/array/make_hash nodes in src/interp/repl.zig:2906-2984 and explicit make_hash in src/testing/compile_chunk.zig:180; focused validation: zig build test -Dtest-filter='Repl JIT roots str_len literal via production collector' passed."
blocks:
  - habu-audit-literal-root-5caaebee
---

Files: src/interp/repl.zig:2779-3020, src/testing/compile_chunk.zig:68-253, src/jit/backend.zig:2244-2444. What: add the currently divergent supported node families (vec_*, str_*, hash_*, arr_new_dyn/arr_set, etc.) to the REPL collector and normalize both collectors to one coverage set. Why: Maxima uses the REPL path; current divergence means production JIT is less safe than test/bench JIT. Verification: diff of handled tags goes to zero for all currently supported nodes.
