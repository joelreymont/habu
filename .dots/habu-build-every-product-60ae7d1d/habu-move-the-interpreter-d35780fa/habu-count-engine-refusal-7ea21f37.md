---
title: Count engine refusal lines from the source, not the evaluate buffer
status: closed
priority: 2
issue-type: task
created-at: "2026-10-26T19:10:40.234100+02:00"
closed-at: "2026-11-10T21:52:35.206282+02:00"
close-reason: "done: landed as 7cc62479, which prints the line only for a file buffer (outer.f:364), the rule of docs/debugging.md:24-28; a refusal inside evaluate prints the file with no :1 on master's engine"
---

E-UNDEFINED inside an evaluated string names <file>:1: src/habu/outer.f LINE (:360-363) counts newlines from SRCLOC:INB-CELL to INP-CELL; same count in habu2.f EM-COMPILE-DIE (~12756) and kernel-x64.f DPBAD-HELPER, (~1565). The top-level site words (trdiag) report the owned span instead. Probe ev2.f in /home/joel/.cache/tmp/carl-trdiag.*.

Census 2026-11-04: an acceptance case of its parent; fix it in the code that stays (named above), never in the machinery the parent replaces.
