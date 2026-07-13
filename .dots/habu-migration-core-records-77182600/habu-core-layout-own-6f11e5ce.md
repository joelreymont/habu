---
title: "Core layout: own early assertion status"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-13T22:46:29.890224+02:00\""
blocks:
  - habu-core-bootstrap-relocate-a54406ac
---

Own the pre-checker core layout assertion exit status independently of legacy structures.f. Rename the earliest src/core/cell.f status to a shared CORE-LAYOUT-RC, use it for CELL width and schema/type-family explicit-layout drift checks, preserve native/recovery load order, and prove focused/fixpoint parity. Required by habu-core-records-remove-31f84baf; no parser, definer, descriptor, or compatibility surface.

Claim: agent=sol workspace=.jj-ws/type-dsl-layout-rc
