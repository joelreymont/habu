---
title: Design T0 joins without a full spill
status: active
priority: 1
issue-type: task
created-at: "2026-10-29T04:43:33.368352+02:00"
---

Plan (Fable high). A5 profile: SPILL-ALL at every begin/if/else/then/while/repeat (t0.f THE VIRTUAL STACK rule, ~106-112) costs 4.18G of the 9.03G zz run gap; dup/over copies (COPY ~468, CMP2 ~428) 0.67G; unfused compare+branch (DO-IF ~696, DO-WHILE) 0.33G. JIT reference: LVSNAP jit.f:911, LVRECON, LVSHARED?, LVFUSE jit.f:657, habu2.f:2707. Design for the shared front end (every arch).
