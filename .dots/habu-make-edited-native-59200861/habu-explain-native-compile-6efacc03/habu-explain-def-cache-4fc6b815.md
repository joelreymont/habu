---
title: Explain definition cache publication refusals
status: active
priority: 2
issue-type: task
created-at: "2026-10-19T13:22:47.709864+03:00"
---

A measured self-build warm replay has874 eligible-cache-miss compile attempts but293/303 path-bearing cases have no stored row, and precompile diagnostics cannot distinguish BEGIN eligibility or checker-busy refusal, OBSERVE capture failure, END refused/not-emitted or FINAL-PRODUCER ordered dependency. Report the actual declining branch through the existing opt-in diagnostic path, with current definition identity and first ordered dependency where relevant, without changing keys or compilation. This bounded measurement must identify the responsible reuse fix; no new loader wrapper or generic diagnostic registry.
