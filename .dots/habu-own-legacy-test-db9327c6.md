---
title: Own legacy test fixtures
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:38:16.944032+02:00"
---

Invariant: every test fixture owns its constants, buffers, variables, helper words, and generated subjects; loading a test must not add state or pseudo-namespaced helpers to a production package or the global dictionary. Package-first enforcement protects new diffs but does not migrate the existing test corpus. Current examples include PT-prefixed property helpers and about thirty AG-prefixed Adam gradient helpers placed inside the broad MAKI owner after that fixture moved under the NanoGPT examples.

Census all test and example fixtures for definitions outside a deliberate test-owned package. Exclude modules already assigned to exact package-migration dots, then split the remaining census into small disjoint leaves by owning suite. Give each fixture a short private package, import shipped production APIs with qualified calls or lexically bounded using blocks, shorten redundant prefixes inside the fixture, and export nothing unless a separate suite entrypoint consumes it. The moved Adam fixture must use its own flat owner and import MAKI; do not wait for or fake nested packages.

Prove every old global or production-private fixture name rejects, production package exports and image bytes do not grow when test fixtures are absent, standalone and co-loaded test order remains deterministic, and every affected exact production suite plus package, namespace, host, and full native gates passes. Record the complete census and leaf ownership before edits so no test-local state is silently skipped. Measure global definition count, dictionary-name bytes, test-only JIT and DATA, and load time before and after; require no unexplained growth.
