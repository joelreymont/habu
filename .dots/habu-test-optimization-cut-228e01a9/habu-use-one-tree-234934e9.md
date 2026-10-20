---
title: Use one tree-copy helper in tests and build tools
status: closed
priority: 3
issue-type: task
created-at: "2026-10-02T19:09:14.965852+02:00"
closed-at: "2026-10-14T19:43:43.593842+03:00"
close-reason: Tree-copy helper shared and duplicate walkers removed in d48702d1; commit records affected builder E2E checks.
---

Problem (lane 362 r4-treecopy, 98106bb2): after test/tree-copy-lib.f (TREE-COPY:FILE, BUILD-SOURCES) three more copies of the checkout-copy walk remain: test/pre-trust-defer.f:88-108 (PARENT-U, COPY-ONE, COPY-ENTRY, COPY-SRC-TREE), tools/build-fixpoint.f:1780-1785 and :1917-1926 (BF-BOOT-COPY, BF-PARENT-U), tools/build-fixpoint-test-lib.f:242-292 (BFT-STALE-COPY-TREE). Acceptance: one helper serves all (in src/lib/ if build tools use it, the test lib then a thin re-export or gone), each copy deleted, its users' tests rc 0. Files: the four above, test/tree-copy-lib.f.
