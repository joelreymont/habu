---
title: Implement sxhash native support
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T14:37:09.948573+02:00"
---

sxhash needs native hash() method but all 256 opcodes used. Options: 1) Add extended hash opcode mechanism, 2) Implement in stdlib using existing primitives, 3) Add native-call mechanism. For now, partially works via Value.hash() but not exposed to Lisp.
