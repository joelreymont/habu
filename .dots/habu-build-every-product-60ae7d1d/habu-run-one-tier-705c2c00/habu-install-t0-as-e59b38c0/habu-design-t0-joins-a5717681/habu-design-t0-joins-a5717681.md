---
title: Design T0 joins without a full spill
status: closed
priority: 1
issue-type: task
created-at: "2026-10-29T04:43:33.368352+02:00"
closed-at: "2026-11-04T11:48:48.453427+02:00"
close-reason: "Census 2026-11-04, done: Design delivered; lanes landed 3e899625, a2e55d06, 387c3a39, e9d1e6b7, a215caf3; remaining case/MATCH joins in f1dcdaa5."
---

Plan (Fable high). A5 profile: SPILL-ALL at every begin/if/else/then/while/repeat (t0.f THE VIRTUAL STACK rule, ~106-112) costs 4.18G of the 9.03G zz run gap; dup/over copies (COPY ~468, CMP2 ~428) 0.67G; unfused compare+branch (DO-IF ~696, DO-WHILE) 0.33G. JIT reference: LVSNAP jit.f:911, LVRECON, LVSHARED?, LVFUSE jit.f:657, habu2.f:2707. Design for the shared front end (every arch).
