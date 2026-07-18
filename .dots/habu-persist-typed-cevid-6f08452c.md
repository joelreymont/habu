---
title: Persist typed cevid evidence rows
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-18T13:22:54.298423+02:00\""
---

Follow-on from habu-v2-competitive-evidence-5d07d471 (closed): add the durable typed store codec for cevid/v1 evidence rows, mirroring how the closed habu-persist-typed-bench-2d15efa2 persisted bench/v1 after the schema landed (maki/competitive-store.f pattern: typed encode/decode, byte-stable round-trip, crash-safe write). The schema + canonical render + byte goldens live in maki/competitive-evidence.f (package CEVID, codes -5417..-5421). Optional second item, decide at claim: wire the sealed unit enum into run-metric MEASURE only if a consumer needs per-metric units at run time (deliberately kept out to avoid resignaturing MEASURE and every caller). Verify: cevid round-trip suite + maki/test.f.

Claim: agent=cevstore workspace=.jj-ws/fable-cevstore (durable cevid/v1 codec per the bench/v1 precedent; units-into-MEASURE only if a consumer needs it)
