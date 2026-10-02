---
title: Refuse ; after def-open at tier 0
status: open
priority: 3
issue-type: task
created-at: "2026-10-04T04:41:56.203496+03:00"
---

Problem (lane 573, on 7c03e870): at tier 0, `def-open` followed by `;` crashes the engine (exit 134, SIGSEGV) when the body calls a word: DEF-OPEN (src/habu/habu2.f ~:4112) sets up no return-address frame, and while `def-close` refuses tier 0, `;` does not. Probe $HOME/.cache/tmp/kestrel-jerry-ctlflow/pr/d0*. Fix: one rule at the responsible layer: either `;` refuses a def-open'ed definition at tier 0 as def-close does, by a named code, or DEF-OPEN builds the frame `;` expects; say which and why. Acceptance: the probe is refused with a named record (rc 70) or compiles and runs correctly, never crashes; seen crashing first; baked: rebuild, g1 == g2 with .names, two-generation build. After: 7a137417.
