---
title: Use one tree-copy helper in tests and build tools
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T19:09:14.965852+02:00"
---

Problem (lane 362 r4-treecopy, 98106bb2): after test/tree-copy-lib.f (TREE-COPY:FILE, BUILD-SOURCES) three more copies of the checkout-copy walk remain: test/pre-trust-defer.f:88-108 (PARENT-U, COPY-ONE, COPY-ENTRY, COPY-SRC-TREE), tools/build-fixpoint.f:1780-1785 and :1917-1926 (BF-BOOT-COPY, BF-PARENT-U), tools/build-fixpoint-test-lib.f:242-292 (BFT-STALE-COPY-TREE). Acceptance: one helper serves all (in lib/ if build tools use it, the test lib then a thin re-export or gone), each copy deleted, its users' tests rc 0. Files: the four above, test/tree-copy-lib.f.
