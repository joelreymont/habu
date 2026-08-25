---
title: "Core layout: own early assertion status"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-13T22:46:29.890224+02:00"
closed-at: "2026-07-13T23:11:12.070380+02:00"
close-reason: Landed CORE-LAYOUT-RC with CELL, exact source pin, schema/family drift assertions, accessor wiring assertions, native fixpoint, Gforth recovery, and all gates green at 4f2caac7.
---

Own the pre-checker core layout assertion exit status independently of legacy structures.f. Rename the earliest src/core/cell.f status to a shared CORE-LAYOUT-RC, use it for CELL width and schema/type-family explicit-layout drift checks, preserve native/recovery load order, and prove focused/fixpoint parity. Required by habu-core-records-remove-31f84baf; no parser, definer, descriptor, or compatibility surface.

Claim: agent=sol workspace=.jj-ws/type-dsl-schema
