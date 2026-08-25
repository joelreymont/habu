---
title: V2 navigation safety evidence contract
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T12:25:28.242901+02:00"
blocks:
  - habu-v2-autonomous-obj-9181cf9c
---

Define the next-stage simulator/HIL contract for temporal tracking and autonomous navigation: sensor/time schemas, state bounds, uncertainty, scenario corpus, deterministic replay, timing/deadline evidence, fault injection, safety envelope, intervention policy, simulator evidence, HIL evidence, and supervised real-world authorization. Acceptance: model accuracy cannot satisfy control safety, static vision evidence cannot satisfy temporal behavior, stale/missing sensor outcomes are typed, and real-world activation is impossible without all required policy evidence.
