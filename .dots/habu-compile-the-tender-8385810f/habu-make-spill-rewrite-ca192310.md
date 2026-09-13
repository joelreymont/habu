---
title: Make spill rewrite and combine linear
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-11T16:38:06.270563+03:00\""
---

Problem: A64SPILL:REWRITE 2.7 s at exponent 1.94 and COMBINE 20.3 s at ops^1.57 (1.21 in the tail); a trivial body with one rewrite pays 2.4 ms in COMBINE because a rewrite rebuilds the module. Acceptance: both fit slopes at most 1.1; a combine rewrite edits in place or rebuilds once per definition; controlled pair. Files: src/compiler/native/spill.f, src/compiler/native/combine.f. Verify: the native suites; tender-perdef scaling fit. Depends: none. Ownership: cedar (combiner) or grant. Claim: unassigned

Parked 2026-09-13 (hazel): the lane's structural work is landed on the root (6cc61f1e: tools/chain-scale.f yardstick with the session-scoped NPROF stopwatch, four spill maps, the descending frame-need fixpoint 25 -> 3 passes); commit 2' ('Plan the combine once per definition', head 172fc17d in .jj-ws/rowan-combine) is parked because its fold arrays baked a dead mapping before the dynamic-buffer registry landed and has not been re-tested on top of 7f5c1209. The acceptance still open is the slope reading, which needs a quiet box (1-minute load under 4, no other hb over 50%): cd .jj-ws/rowan-root && ./bin/hb --load tools/chain-scale.f (add -- 1.1 for the ratchet) and tools/compile-floor.f; the only reading on record was taken at load 12 (combine 666 spill-line 840 spill-frame 1595) and is not quotable; spill-frame's slope is unexplained (FRAME-ARG-PATH? measured linear and its memoization was dropped as slower).
