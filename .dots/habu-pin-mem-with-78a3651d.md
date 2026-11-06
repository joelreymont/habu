---
title: "Pin MEM:WITH-BYTES zeroing with a test"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T03:59:53.812931+02:00"
---

Problem: MEM:WITH-BYTES (src/lib/memory.f) zeroes the bytes it lends, and no test fails if it stops (stuart, hbind fix, 2026-11-07). Acceptance: a case in an existing memory suite that reads the lent bytes after a previous lender dirtied them and expects zeros. Files: src/lib/memory.f (read), the memory suite under test/. Verify: that suite. Depends: none. Ownership: carl. Claim: unassigned.
