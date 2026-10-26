---
title: Count engine refusal lines from the source, not the evaluate buffer
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T19:10:40.234100+02:00"
---

E-UNDEFINED inside an evaluated string names <file>:1: src/habu/outer.f LINE (:360-363) counts newlines from SRCLOC:INB-CELL to INP-CELL; same count in habu2.f EM-COMPILE-DIE (~12756) and kernel-x64.f DPBAD-HELPER, (~1565). The top-level site words (trdiag) report the owned span instead. Probe ev2.f in /Users/joel/.cache/tmp/carl-trdiag.*.
