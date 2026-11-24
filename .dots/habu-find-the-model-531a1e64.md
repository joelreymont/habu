---
title: "Find the model suite's intermittent throw"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T17:16:11.015551+02:00"
---

Problem: src/lib/net/model-test.f:4823 MODEL-TEST:RUN throws in about 1 of 6 runs of the model suite alone on 37e207b0, MODEL:E-TRANSPORT (-9703) once and MODEL:E-RESPONSE (-9705) once, with nothing printed before it. Acceptance: the cause found and fixed; the model suite passes repeated runs. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/bob-modelfail.
