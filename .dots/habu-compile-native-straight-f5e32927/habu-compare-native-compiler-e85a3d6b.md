---
title: Compare native compiler shadows
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:57:52.866441+02:00"
blocks:
  - habu-run-isolated-native-dda7b8bc
  - habu-add-compiler-shadow-af513a3c
---

Full context: design sections 14.3 and 15 require old/new compilation and isolated execution comparison for every supported definition. Compare compile/reject outcome, diagnostics, stdout/stderr, exit, stack result, metadata, and coverage; unsupported inputs name a capability. Acceptance: SQUARE/arithmetic/stack fixtures agree; a hidden fallback, coverage omission, or new-path publication fails. Dependency: isolated native runner and Wave 0 shadow plumbing.
