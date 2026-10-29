---
title: "Carry the JIT's code techniques into T0"
status: active
priority: 1
issue-type: task
created-at: "2026-10-29T09:56:25.814690+02:00"
---

Joel 2026-10-29: finish T0 by taking in the best of the JIT, then retire it. Census of JIT vs T0 at mxsxwkkn (26 rows, probes compiled under both and disassembled): T0 matches the JIT on the virtual stack, shared copies, registers across if/else and begin loops, fused compare-and-branch, movz/movk literals, locals, spills at calls and case joins, and is ahead on bitmask immediates, cmn, j, rot/tuck and batched spills. Absent or partial rows are the children here plus do loops (habu-t0-joins-for-f1dcdaa5). Phase measurement on zz (instructions, 3 runs): run alone JIT 12.722G vs T0 12.746G (+0.19%); load 3.129G vs 3.488G; the load gap is fact fetch 26%, T0-only plumbing 24%, shared checker words 23%, publish 16%, T0 compiler 13%.

Fill steering (kxrvkxuy, jerry-t0j7): a value T0 fills from memory takes the vreg the innermost open layout names for its slot counted from the bottom, with the lower slots' vregs held back, as jit.f NEED-ALLOC, steers a pull. Top alignment (the first try) moved nothing; depth tracking was not adopted because the JIT's rule is bottom alignment. A do loop with a call keeps its sum in the head's register (row STEER-CASE). Left: `swap 10 * +` after a call still moves at the back edge, because the add writes its deeper operand's register, which is the JIT's choice too (LVDEST).
