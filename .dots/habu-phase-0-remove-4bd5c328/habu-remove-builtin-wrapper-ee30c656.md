---
title: Remove builtin wrapper synthesis
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.074447+02:00"
blocks:
  - habu-make-lookup-apis-32266bf5
---

Problem: builtin function designators still synthesize wrapper lambdas and eval-based call paths. Acceptance: builtins resolve as canonical first-class callables or direct VM dispatch with no eval wrappers. Files: src/interp/repl.zig:1435-1632, src/compiler/compile.zig:8241-8364,18931-18933. Verify: funcall/apply/symbol-function regressions and rg for wrapper lambdas calling eval. Blockers: habu-make-lookup-apis-32266bf5; also depends on habu-remove-legacy-lookup-e81bb093.
