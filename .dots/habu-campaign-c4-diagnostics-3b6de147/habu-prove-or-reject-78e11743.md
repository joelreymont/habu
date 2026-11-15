---
title: "Prove or reject the storage grid's comma-run checks"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:35:37.170104+01:00"
closed-at: "2026-11-16T04:07:34.850650+01:00"
---

Problem: checker.f CHECKER-STORAGE-GRID still throws a bare 7121 for count <= 0, off < 0 and an unaligned off, which rest on how habu1.f BCOMMA (:1798-1806), interpreter.f DEF-ANCHOR (:2424) and the linker restore keep the comma-run cells. Acceptance: each is shown unreachable and deleted, or writes a record, rc 70. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w49grid.
