---
title: Fix stdlib InvalidSyntax after line 498
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-17T11:22:34.741262+02:00\""
---

src/compiler/compile.zig or src/interp/repl.zig: After loading first 498 lines of stdlib.habu, coerce function fails to compile with InvalidSyntax error. Works fine in isolation. Likely compiler state corruption or macro table issue. Bisected to line 499 (closing of decf macro). Need to debug why cumulative loading causes compilation failure.
