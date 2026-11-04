---
title: "Compile create's body at tier 0 on arm64"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-04T13:51:16.717770+02:00"
closed-at: "2026-11-04T19:43:38.271783+02:00"
close-reason: "landed: f7e597db 22ce649d ca0cb629 715f92ad 92b7dbcc on 3378e2d6. Proof at the tip candidate (/tmp/jerry-step1-land/hb, built by the installed hb, rc 0): compiler-shadow row (shadow.f then shadow-kinds.f) rc 0, 29 records ok, 0 stale; full suite ran 521 of 521, one red: pg (pg-kill-test REPORT-READ READY-MS 60 s expired under load, rc 67 -2502, the known case in habu-remove-the-test-6fc078db), which passes alone twice on the same candidate (5.3 s). Reviews: kinds-a, kinds-b, evalcell and the tip ACCEPT after folds."
---

Step 1 lane kinds-a. definers.f DEF-ADDR-TAIL drops its x86-64 split, so the typed loop's create, variable, BUFFER:, TYPED-VARIABLE, DYNAMIC-BUFFER and a does> child file a T0 shadow on arm64 (shadow-kinds measured -6616 for each, tier 1's SHADOW-FIXED having no unplaced arm64 emission); shadow-kinds counts an EXPORT alias sharing its source's routine as ok. Workspace .jj-ws.noindex/jerry-kinds-a on 35ae4a04; brief /tmp/jerry-big/kinds-a-brief.md.
