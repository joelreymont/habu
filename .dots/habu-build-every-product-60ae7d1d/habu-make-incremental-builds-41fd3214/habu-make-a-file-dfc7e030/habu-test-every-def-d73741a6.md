---
title: Test every definition kind under an arm64 shadow
status: closed
priority: 2
issue-type: task
created-at: "2026-11-04T13:38:14.351161+02:00"
closed-at: "2026-11-04T19:43:38.257886+02:00"
close-reason: "landed: f7e597db 22ce649d ca0cb629 715f92ad 92b7dbcc on 3378e2d6. Proof at the tip candidate (/tmp/jerry-step1-land/hb, built by the installed hb, rc 0): compiler-shadow row (shadow.f then shadow-kinds.f) rc 0, 29 records ok, 0 stale; full suite ran 521 of 521, one red: pg (pg-kill-test REPORT-READ READY-MS 60 s expired under load, rc 67 -2502, the known case in habu-remove-the-test-6fc078db), which passes alone twice on the same candidate (5.3 s). Reviews: kinds-a, kinds-b, evalcell and the tip ACCEPT after folds."
---

Cross-build step 1, first lane (tests only). The build loads product sources through the typed loop at T0 under a plain arm64 shadow (NABI:BINDING NSHADOW:OPEN): T0 already walks every T0 body twice, shadow unplaced then host placed, so a plain shadow needs no arm64 native mode. Lane: test/compiler/shadow-kinds.f defines one word of every kind through the typed loop and checks each record's shadow emission equals its host routine but for call fields; reports which kinds fail (expected from reading: create/variable via def-create, cast:/linear: via def-cast). Brief /tmp/jerry-big/kinds-brief.md; workspace .jj-ws.noindex/jerry-kinds; agent=jerry worker.
