---
title: A qualified name is covered by another package's mark
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T15:00:01.988188+02:00"
---

Problem: src/core/checker.f:13530 UNSEEN-COVERS? covers a qualified name when its package's public mark covers it, and when that package has no mark it falls through to the current package's marks, so a qualified OTHER:NOPE inside a marked package counts as covered (code reading, not run). A package's mark covers names that package may define; it says nothing about OTHER's. No doc states the mark's reach across packages. Acceptance: a qualified name is covered only by its own package's public mark; test/signature-nesting.f holds a qualified covered name, which defers, and a qualified name of an unmarked package inside a marked one, which is E-UNDEFINED, each under the load, the check and --verify-only. Depends: none. Claim: unassigned.
