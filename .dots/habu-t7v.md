---
title: Implement native apropos function in Habu
status: closed
priority: 3
issue-type: task
assignee: ""
created-at: "2025-12-04T08:50:01.600142+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

Implement a native apropos function in Habu that searches the symbol table for symbols matching a pattern. Then add it as an MCP tool so it can be used from the REPL and for introspection. Currently apropos uses SBCL's symbol table - need a native version for self-hosting.
