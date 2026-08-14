---
title: Compare native compiler shadows
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:57:52.866441+02:00\""
closed-at: "2026-08-14T12:05:48.153178+02:00"
close-reason: "Closed SUPERSEDED (Wave-0/2 audit 2026-08-14): the straight-line mirror of f8b5d801's closure - comparison satisfied strictly-more (judge board + fuzz oracle + census), the SQUARE fixture is the migrate suite's first case and re-proved live today through the production entry, unsupported inputs name capabilities, coverage omission fails the gate; the isolated-execution and new-path-publication-fails clauses are the pre-cut world. The judge-red seen during audit was the STALE-BINARY lesson (fresh install on tip: green)."
blocks:
  - habu-run-isolated-native-dda7b8bc
  - habu-add-compiler-shadow-af513a3c
---

Full context: design sections 14.3 and 15 require old/new compilation and isolated execution comparison for every supported definition. Compare compile/reject outcome, diagnostics, stdout/stderr, exit, stack result, metadata, and coverage; unsupported inputs name a capability. Acceptance: SQUARE/arithmetic/stack fixtures agree; a hidden fallback, coverage omission, or new-path publication fails. Dependency: isolated native runner and Wave 0 shadow plumbing.
