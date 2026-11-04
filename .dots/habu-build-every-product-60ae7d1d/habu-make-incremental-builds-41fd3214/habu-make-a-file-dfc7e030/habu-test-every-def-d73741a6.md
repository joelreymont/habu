---
title: Test every definition kind under an arm64 shadow
status: active
priority: 2
issue-type: task
created-at: "2026-11-04T13:38:14.351161+02:00"
---

Cross-build step 1, first lane (tests only). The build loads product sources through the typed loop at T0 under a plain arm64 shadow (NABI:BINDING NSHADOW:OPEN): T0 already walks every T0 body twice, shadow unplaced then host placed, so a plain shadow needs no arm64 native mode. Lane: test/compiler/shadow-kinds.f defines one word of every kind through the typed loop and checks each record's shadow emission equals its host routine but for call fields; reports which kinds fail (expected from reading: create/variable via def-create, cast:/linear: via def-cast). Brief /tmp/jerry-big/kinds-brief.md; workspace .jj-ws.noindex/jerry-kinds; agent=jerry worker.
