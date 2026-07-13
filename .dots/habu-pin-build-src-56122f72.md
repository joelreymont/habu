---
title: Pin build source bytes
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T03:14:28.582901+02:00"
---

Full context: pending tools/build-fixpoint.f hashes a source path then reopens it for append, creating a TOCTOU gap; generic build extensions also retain caller scratch path pointers. Open once and pin bytes or descriptor identity, own/copy extension configuration, and prove mutation during build cannot change certified input.
