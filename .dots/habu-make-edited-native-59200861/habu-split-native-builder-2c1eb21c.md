---
title: Split native builder and unit entry
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-29T21:14:55.093888+02:00\\\"\""
closed-at: "2026-09-30T12:09:03.922625+02:00"
close-reason: Ordinary builder no longer eagerly loads unit modules; explicit unit entry owns preflight/import/export. Identity-only donor builds merged source; current-source NBR actual hit and hb/names byte parity, stale refusal and fresh checker rejection passed; saved builder and499/499 native gate/convergence green. Astra independent reviews PASS.
---

Current tools/native-build-core.f eagerly loads source-view, unit compiler and object modules even for ordinary cold builds, so an identity-only host cannot build a combined source lacking NBR host primitives. Keep ordinary native-build.f and its core independent of cache-only modules; add an explicit unit entry that owns source preflight, NBR import/export and capability needs through narrow loader/writer hooks. Before code, add E2E acceptance showing identity-only donor can source-load the ordinary builder and build the merged source, while the unit entry preserves actual hit, edited-client cold/import hb and .names byte parity, stale-source refusal/no output and fresh checker rejection. Verify compile-only loads during the active gate; root owns full gate/convergence and integration.
