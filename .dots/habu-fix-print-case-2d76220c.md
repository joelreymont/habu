---
title: Fix print-case keyword dispatch
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:05:13.669595+01:00"
---

Context: src/runtime/primitives/io.zig:784-796; cause: std.mem.eql on keyword names; fix: compare against interned keyword identities (builtins kw_upcase/downcase/capitalize); deps: habu-optimize-typep-dispatch-61507cab; verification: add print-case test, run zig build test --filter io
