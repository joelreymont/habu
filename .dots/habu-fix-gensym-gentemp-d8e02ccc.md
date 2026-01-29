---
title: Fix gensym/gentemp fallback uniqueness
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:05:31.032784+01:00"
---

Context: src/runtime/primitives/symbol.zig:29-86; cause: bufPrint catch returns constant string causing duplicate names; fix: return error or allocate dynamic buffer to ensure unique names; deps: none; verification: add gensym/gentemp uniqueness test, run zig build test --filter symbol
