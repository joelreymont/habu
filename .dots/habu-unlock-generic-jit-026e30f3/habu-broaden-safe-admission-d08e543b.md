---
title: Broaden safe admission without opt-decl dependency
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-22T20:19:57.316164+01:00\""
closed-at: "2026-02-22T20:33:17.203775+01:00"
close-reason: completed
---

src/jit/candidates.zig + src/interp/repl.zig + src/testing/compile_chunk.zig: remove stale strictness requiring explicit speed3/safety0 when translator can preserve semantics; no Maxima-specific special casing.
