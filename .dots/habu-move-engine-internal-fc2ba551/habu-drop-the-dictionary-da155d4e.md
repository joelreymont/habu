---
title: Drop the dictionary high-water if nothing reads retired records
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T16:06:12.805693+01:00"
---

Problem: NDICT-HW-CELL (src/habu/layout.f:892) holds the largest count NDICT was lowered from. CLEAR-KIND (src/habu/habu2.f:561) scans records up to it, and the child-region copy (src/habu/habu1.f, the used-extent code reading NDICT-HW-CELL) copies records and code up to it. Once ndict! has no raise path (every caller lowers), no record above NDICT becomes live again, so both readers may serve no input. Acceptance: NDICT-HW-CELL and its uses are deleted, or each reader that needs a retired record is named as path:line with the program input that reaches it. Depends: none. Claim: unassigned.
