---
title: Add signal handling for native code
status: closed
priority: 2
issue-type: feature
assignee: ""
created-at: "2025-12-03T19:53:09.725088+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

Implement signal handlers (SIGSEGV, SIGBUS, SIGFPE, etc.) in native Habu code. Required for: graceful crash handling, breakpoint support (SIGTRAP), stack overflow detection. Use sigaction() via extern-call.
