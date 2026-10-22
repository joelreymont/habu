---
title: Explain definition cache publication refusals
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T13:22:47.709864+03:00"
closed-at: "2026-10-22T12:36:33.982708+03:00"
close-reason: "Triage 2026-10-22, DONE (STALE-CLAIM): native-unit-object.f:99 \"cache-store-refused reason=\" (:692 capture-refused/no-emission); the reason=checker-busy refusal is what 22488687 cites, and that follow-up fix is filed there."
---

A measured self-build warm replay has874 eligible-cache-miss compile attempts but293/303 path-bearing cases have no stored row, and precompile diagnostics cannot distinguish BEGIN eligibility or checker-busy refusal, OBSERVE capture failure, END refused/not-emitted or FINAL-PRODUCER ordered dependency. Report the actual declining branch through the existing opt-in diagnostic path, with current definition identity and first ordered dependency where relevant, without changing keys or compilation. This bounded measurement must identify the responsible reuse fix; no new loader wrapper or generic diagnostic registry.
