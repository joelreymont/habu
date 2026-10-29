---
title: "Carry the JIT's code techniques into T0"
status: active
priority: 1
issue-type: task
created-at: "2026-10-29T09:56:25.814690+02:00"
---

Joel 2026-10-29: finish T0 by taking in the best of the JIT, then retire it. Census of JIT vs T0 at mxsxwkkn (26 rows, probes compiled under both and disassembled): T0 matches the JIT on the virtual stack, shared copies, registers across if/else and begin loops, fused compare-and-branch, movz/movk literals, locals, spills at calls and case joins, and is ahead on bitmask immediates, cmn, j, rot/tuck and batched spills. Absent or partial rows are the children here plus do loops (habu-t0-joins-for-f1dcdaa5). Phase measurement on zz (instructions, 3 runs): run alone JIT 12.722G vs T0 12.746G (+0.19%); load 3.129G vs 3.488G; the load gap is fact fetch 26%, T0-only plumbing 24%, shared checker words 23%, publish 16%, T0 compiler 13%.
