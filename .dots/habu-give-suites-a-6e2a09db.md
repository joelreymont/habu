---
title: Give suites a required-capability outcome
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T11:22:30.779049+02:00"
---

The test framework cannot express 'this suite requires host capability X': every suite hand-rolls a precondition (kv-cache-test now dies 74 with prose) and a missing capability is indistinguishable from a failed assertion in the runner's report. A suite-level TEST:REQUIRE that reports 'precondition unmet: <name>' as its own outcome kind — distinct from pass and fail, counted and named in the report, never silently green — would have made the kv-cache red self-naming from day one. Design it once in lib/test.f; convert kv-cache-test's hand-rolled die as the first consumer.
