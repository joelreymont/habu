---
title: growth paths leak the previous mapping
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.927556+02:00"
---

Problem: lib/json-write.f:84-87 JW-GROW allocates, copies, installs and never releases the old span (every doubling leaks); same shape lib/build-cache.f:246-251 SELECT-BUF and lib/test/src-shape.f:42 ALLOC (one span per LOAD). lib/vector.f:373-379 and byte-buffer.f:791-797 show the correct copy-install-release order. Acceptance: each releases the old span after install; a test grows past two doublings and asserts the mapping count. Files: lib/json-write.f, lib/build-cache.f, lib/test/src-shape.f. Verify: the tests. Depends: none. Ownership: lib memory. Claim: unassigned.
