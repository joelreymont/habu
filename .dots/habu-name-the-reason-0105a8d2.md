---
title: Name the reason when a body char has no operand
status: open
priority: 3
issue-type: task
created-at: "2026-10-29T04:30:09.950058+02:00"
---

A body ending in 'char' or '[char]' (or the preflight's target-last body for an immediate user 'char', /tmp/jerry-chk/rev/p/c1.f) is refused by SKIP-PARSE-LIT-PAYLOAD (src/core/checker.f ~21407) setting OK and PARSE-COMPLETE to 0 with no code or reason: 'habu: in e: at char', exit 70. Give it a named diagnostic. Workspace: none yet.
