---
title: "Keep the record writer out of a program's uncaught throw"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:43:01.376976+01:00"
---

Problem: src/habu/app-end.f requires src/lib/error.f and ENTRY calls `rc ERROR:UNCAUGHT`, whose OPEN and HELP link REJECTION's record writer into every stripped program: test/aot-shifted-bytes.f's GAP program grows from 65744 to 131280 bytes (text end 0x7fe2 to 0x12dea, +44.5 KB). Acceptance: a stripped program's uncaught throw still names its code and text, and the GAP program's link map holds only the words a placeless record writes; its size is measured against 65744. Depends: none. Claim: unassigned.
