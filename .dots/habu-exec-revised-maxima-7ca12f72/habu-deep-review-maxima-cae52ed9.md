---
title: Deep review Maxima parity bootstrap/runtime surfaces
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-03-07T20:15:20.397541+01:00\\\"\""
closed-at: "2026-03-07T21:12:06.904403+01:00"
close-reason: done (deep review completed with parallel code-auditor/edge-case-hunter/scout + reviewer verification. Findings mapped to existing dots habu-delegate-composite-stream-9553a1f8, habu-finish-composite-stream-638bd128, habu-adopt-canonical-test-a8a0cbe4, habu-audit-and-harden-5576b7ee, habu-harden-handler-restart-d526af6a and new dots habu-remove-broken-mset-a1734f45, habu-complete-src-tree-0cc82017. First fix batch landed in io.zig/repl.zig/compile.zig/opcodes.zig/stdlib.habu/maxima-stubs.lisp with focused validation.
---

Deep-review required before further Maxima parity implementation. Audit file groups with explicit evidence: lib/maxima-post-load.lisp:1-220, lib/maxima-loader.lisp:1-180, lib/maxima-stubs.lisp:1-380, src/runtime/primitives/io.zig:1600-2300, src/interp/vm.zig relevant stream/handler/restart areas, lib/stdlib.habu:6170-7210, tools/maxima-rtest.lisp:1-120. Produce findings table, plan fix dots under the canonical root, then implement critical/major findings before resuming remaining dots.
