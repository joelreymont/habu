---
title: Exhaust the host return stack as native does
status: open
priority: 2
issue-type: task
created-at: "2026-10-10T09:25:38.829991+03:00"
blocks:
  - habu-report-machine-stack-42704bbe
---

Problem: on the Gforth host one Gforth return stack (default 3968 cells) holds what native keeps on two stacks: call frames, which native keeps on its machine stack, and `>r` cells, which native keeps on its guarded return stack (STACK-ABI:RETURN-CELLS, 8192). So deep recursion exhausts the host first in a different place and ends differently: native's own overflow shape (`begin dup recurse again`, test/engine-stack-jit.f RATCHET; ov6 in ~/.cache/tmp/carl-gfrest/handoff.md "Session 19") is `hb: stack bounds exceeded (data)` rc 102 on native and a catchable `hb: uncaught throw code -9` rc 67 on the host. The host's data stack already ends as native's (habu-hand-the-rest-32631946: guard pages per closed text, `hb: stack bounds exceeded (data)`).
Acceptance: on the host, call depth and `>r` cells are bounded as native bounds them: each exhaustion is fatal with native's message for that stack and rc 102, never a catchable throw, and the capacities come from native's layout and the native machine stack's extent, not Gforth's defaults. Cases in test/gforth/cases/ match native: RATCHET's shape, pure recursion (c1, c2 in ~/.cache/tmp/carl-rsov/), recursion that parks a `>r` cell per level, each under `catch` too.
Files: src/host/gforth/ (the launcher's stack sizes, layout.fs's fault handler), test/gforth/cases/, test/gforth/host-test.f.
Verify: `HB_TMP=$PWD/build/tmp bin/hb --load test/gforth/host-test.f`.
Depends: habu-report-machine-stack-42704bbe (native's machine-stack message); builds on habu-hand-the-rest-32631946's guard pages. Worker: worker-max.
