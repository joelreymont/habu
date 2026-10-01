---
title: Correct the uncaught-throw exit in debugging.md
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T11:32:11.986272+02:00"
---

Problem: docs/debugging.md:138 says an uncaught throw exits with the code's low eight bits and prints nothing. Measured: a code of 1..255 exits with that code silently; any other code prints 'hb: uncaught throw code N' on stderr and exits 67 (r4-pg commit 4 corrected docs/gate.md the same way). Acceptance: debugging.md states the measured rule and agrees with docs/gate.md.
