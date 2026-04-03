---
title: Canonicalize loader specials
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.109977+02:00\""
closed-at: "2026-04-03T09:12:25.923317+02:00"
close-reason: done (zig build; zig build test back to known 5-error baseline)
blocks:
  - habu-canonicalize-pkg-state-4641836b
---

Problem: load path/package context is repaired by alias writes and cwd heuristics instead of dynamic special binding. Acceptance: load truthfully binds *LOAD-PATHNAME*, *LOAD-TRUENAME*, *DEFAULT-PATHNAME-DEFAULTS*, and *PACKAGE* for nested loads. Files: src/interp/repl.zig:2038-2066,2116-2149,2216-2237; lib/stdlib.habu:7319-7347. Verify: nested load/autoload probes prove dynamic state propagation and fail closed on real translation errors. Blockers: habu-canonicalize-pkg-state-4641836b.
