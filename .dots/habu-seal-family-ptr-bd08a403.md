---
title: Seal family pointer provenance
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T12:43:48.961313+02:00"
---

Critical soundness: src/core/checker.f LAYOUT-PTR-BIND-OK? lets an empty ( ptr a -- ptr family ) definition launder raw pointers and cross family identity. Replace generic pointee binding with a generative family-typed storage/accessor mechanism; add raw-pointer cast, same-address/two-family, and valid accessor regressions. Dependency: wide ADT memory S2 must not merge until closed.
