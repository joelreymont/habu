---
title: Revalidate the September audit against the native integration candidate
status: open
priority: 1
issue-type: task
created-at: "2026-09-14T13:33:29.234952+03:00"
---

User audit /tmp/habu-audit-2026-09-14.md targets old c3e1b024 source and Sept10 engine, not integration. Current recheck: C2 quotation variance, C4 implicit return reads, M8 signature tails reproduce and have separate dots; C1 tested bad programs reject; listed C3 caller exploits reject but broader row checks untested; C6 dispatch and M10 gate early-stop superseded. Current source still suggests M1/M2 stack bounds, M3 public ndict bound, M4 integer overflow, M9 foreign IR successors, C5 stripped quotation addresses, M11 environment cap, M13 zero unmap. Revalidate remaining correctness claims before marking resolved or importing old counts. Performance P1-P11 deferred by user. Read RESTART.md for candidate hashes, original probes, and limits.
