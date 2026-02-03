---
title: Fix compiler env keys
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-03T10:40:32.454471+01:00\""
closed-at: "2026-02-03T15:04:25.782724+01:00"
close-reason: Key env by symbol identity (pkg/uid); fix captures; add tests
blocks:
  - habu-fix-macro-chunk-5f196625
---

src/compiler/compile.zig:1272-1370: Env uses StringHashMap keyed by symbol print-name; loses package identity + can collide for distinct symbols with same name. Fix: key env by symbol Value identity; propagate through compileVar/let/lambda; update boxing/type envs similarly (avoid name-based collisions). Add tests for package-qualified locals and shadowing. Verification: zig build test.
