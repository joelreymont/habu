---
title: Regress reopen name resolution
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T17:25:37.562438+02:00"
blocks:
  - habu-share-reopen-name-92885254
---

Leaf (b) of the reopen-name-resolution parent. Blocked on the shared-resolution-authority leaf; it pins that leaf's result and adds nothing of its own to the resolver.

Owned result: a negative regression that fails on the pre-change engine and passes after, built from the parent's two-file reproducer rather than from a paraphrase of it. It must prove the specific soundness property: for a bare reference inside a body that REOPENS a package, in a file other than the one that defined the package, where the referenced tail case-insensitively shadows a core primitive, the checker's certificate and the compiler's binding agree. The check runs through the real CHECK! and bin/hb path.

Hostile cases the regression must include, because a substring or single-happy-path test would pass without the fix: the shadowing name appearing in a comment and in a string literal inside the reopened body (neither may affect resolution); the same tail referenced both bare and package-qualified in one body; the definition and the reopen in the opposite file order; and a control case where the package tail does NOT shadow anything, which must stay unchanged.

Acceptance: the regression is red on the tree before the shared-authority leaf and green after, with the exact diagnostic or the exact runtime value recorded in both directions; it lives with the checker suites and runs in their owning gate; both diff lints pass.
