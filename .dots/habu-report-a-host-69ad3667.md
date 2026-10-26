---
title: "Report a host's missing trusted primitive as undefined"
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T09:35:45.373860+02:00"
---

When a host engine lacks a primitive the tree declares trusted-only (master ffi-call-struct-bounded on an older host), the build fails E-CAP-TRUSTED in CALL-STRUCT rather than naming the missing word, hiding the cause. Found by carl host lane (workspace carl-host); repro: older tip engine T building master 6981fddd, rc 74.
