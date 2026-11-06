---
title: "Record a local's width once a later call resolves it"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T03:59:53.785709+02:00"
---

Problem: a local bound while its cell's width is open records width 1 even when a later call resolves the width: 'OPTION:NONE {: x :} x KEEPO drop x' with KEEPO ( n n option<n> -- n n option<n> ) (stuart, hbind fix, 2026-11-07; untested). Verify first: run it and count the depth. Acceptance: x reads the resolved width or the program is refused; a test. Files: src/core/checker.f, test/wide-typed-local-probe.f. Verify: that test. Depends: none. Ownership: carl. Claim: unassigned.
