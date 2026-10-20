---
title: Bound definition cache edit history
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T10:36:40.351413+03:00"
---

Automatic definition containers keep superseded rows across edits: STORE appends variants and each load reads/indexes the entire stable-path container. Retain only rows consumed or produced by a successful file load, atomically replacing its container; failure keeps the previous complete container. Preserve current unchanged definitions and cache format. Verify real repeated source edits reuse current definitions without cache size growing with history, executable output and failure recovery.
