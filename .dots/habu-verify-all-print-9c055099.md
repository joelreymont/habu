---
title: Verify all ✓ print variables
status: open
priority: 2
issue-type: task
created-at: "2026-01-18T06:24:31.783842+02:00"
---

Files: src/runtime/primitives/io.zig, stdlib.habu
cl-symbols.md claims *print-escape*, *print-case*, *print-length*, etc. are implemented.
Grep each one, verify they're actually wired up and functional.
Create list of false claims.
Verify: Document findings in /tmp/print-var-audit.txt
Est: 25min
