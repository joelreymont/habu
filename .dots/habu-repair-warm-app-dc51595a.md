---
title: Repair warm app cache refusal
status: active
priority: 2
issue-type: task
created-at: "2026-10-21T09:48:33.214513+03:00"
---

Public e34aafe1/a30+7557 unmodified Maki602e32c3 build exits76 with empty output; same source and byte-identical app maker succeed with fresh cache. Saved-maker reduction catches76 insideNFILE-CACHE:APP-LOAD before startup/save after8224hits/5385body-skips/269compiled. Preserved151APFS-cloned cache entries and exact commands in /Users/joel/Work/maki/.jj-ws.noindex/tim-build-diagnosis/build/build-handoff.md. Cache came from an earlier failed diagnostic load; earlier valid entries must remain safe. Reduce first bad entry/owner state, fix the responsible cache publication/import boundary, retain valid warm hits and exact runnable cold/warm products. No blanket cache clearing, swallowed failures, permissive import, caller edits or private public-engine substitution. Existing behavior test first if a distinct gap; independent Astra review and proportionate native/edit/convergence qualification for any capture/checker changes.
