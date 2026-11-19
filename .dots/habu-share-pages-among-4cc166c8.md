---
title: Share pages among small MEM blocks
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T12:19:34.700582+02:00"
---

Problem: in an idle program built with Habu (Omabox's daemon), 19 small blocks each held a whole page: 0.24 MB, 0.2 MB of it page padding. BUF and the AIO group row now take small blocks from HEAP:ALLOCATOR; the measurement remains. Acceptance: the idle Omabox daemon's small blocks hold little more than their contents, measured in /proc/PID/smaps. Depends: none. Claim: agent=bob.
