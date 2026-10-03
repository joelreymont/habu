---
title: Compile an empty quotation at tier 1
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T20:58:01.329971+03:00"
---

On master, `: W ( n -- n ) [: ;] drop ;` fails at tier 1 with 'ncomp: cannot compile W at [:' (throw -8651), even with no locals or SKIP; it compiles at tier 0. Found while landing habu-load-authored-src-4ef714a3 (sealport/logs/p2inv-master.txt, w-quot and w-quot-scalar).
