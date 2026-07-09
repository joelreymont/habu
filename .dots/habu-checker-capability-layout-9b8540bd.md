---
title: "Checker capability: layout-polymorphic family params"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.964243+02:00"
---

Follow-on capability, not in PLAN.md v1: item 12 rejects layout types in cell-only family parameters, so span<result<...>>/containers of ADTs are inexpressible. Add layout-polymorphic parameter kinds (families declaring param kinds that accept layout values with width propagation into instantiation and lowering) so collections of sums/products type-check. Needs: param-kind metadata (TFAM 2a records), width-aware instantiation (TFAM 12 machinery), negative fixtures flip to positive. Depends: TFAM 12, 16.
