---
title: "Settle CODE-RECLAIM's alias guard for build rewinds"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T19:06:15.287183+01:00"
---

Problem: CODE-RECLAIM:FLOOR-FROM (src/habu/xref.f:750) keeps LIVE-SCAN (:729) and MIN-ABOVE-LIVE (:735) so a cut never frees an arena routine a surviving alias record points at (header paragraph from :681). Its one caller is now the linker's FORGET-SPACE (src/habu/linker.f:5776); the deleted XREF:FORGET-DEFS-FROM was the path that made such a record, and by reading a build cannot (LINK-ALIAS sources are the same build's records, MAKE-OWNED records name kernel text below ARENA-LO, a build resolves in fresh wordlists). Nothing tests the alias case. Acceptance: either a test/linker.f case in which a build makes an alias record of a routine that survives its close, checked by HOST-CHECK (:99), or LIVE-SCAN and MIN-ABOVE-LIVE give way to the plain floor and the alias paragraph goes. Depends: none. Claim: unassigned.
