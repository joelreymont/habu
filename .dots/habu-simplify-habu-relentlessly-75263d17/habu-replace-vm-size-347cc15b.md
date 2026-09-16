---
title: Replace VM-size leak ratios with exact mapping witnesses
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-08-23T18:41:29.996383+02:00\\\"\""
closed-at: "2026-09-16T14:34:50.321843+03:00"
close-reason: "done: lib/test/vmsize.f and its ratio thresholds are gone and the mapping witness is the replacement [lib/test/mapped.f:52 LIVE?; no vmsize.f under lib/test]"
---

Use MAPPED:LIVE? for source/build growth release checks and delete lib/test/vmsize.f and ratio thresholds.
