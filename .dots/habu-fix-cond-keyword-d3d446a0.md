---
title: Fix condition keyword dispatch
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:05:18.559439+01:00"
---

Context: src/runtime/primitives/condition.zig:20-35; cause: std.mem.eql on keyword names; fix: compare to interned keyword identities (add kw_format_control/kw_format_arguments); deps: habu-fix-print-case-2d76220c; verification: add condition initargs test, run zig build test --filter condition
