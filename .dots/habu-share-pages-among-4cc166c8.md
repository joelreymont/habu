---
title: Share pages among small MEM blocks
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T12:19:34.700582+02:00"
---

Problem: src/lib/memory.f:43-44 MEM-ALLOC-PTR maps a fresh anonymous region for every block, so on a 16 KiB-page machine a block of a few bytes holds a whole page; in an idle program built with Habu (Omabox's daemon) 19 such mappings hold 0.24 MB, 0.2 MB of it page padding. Acceptance: blocks much smaller than a page share pages, releasing a block still releases what it held, and an idle built program's small blocks hold little more than their contents, measured. Depends: none. Claim: unassigned.
