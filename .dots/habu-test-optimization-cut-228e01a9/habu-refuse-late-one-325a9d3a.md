---
title: Refuse late one-shot lifecycle registrations
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T15:12:49.879680+02:00"
closed-at: "2026-09-30T15:13:06.397091+02:00"
close-reason: Landed cc9ae821 (E-LIFECYCLE-LATE -9310, lib/errors.f block -9310..-9319). Fable review accepted. test/image-lifecycle-late-register.f fails on the old engine, passes on the candidate; engine built by tools/native-build.f. Native gate 504/504 rc 0, 281.2 s wall, 1984 s pooled at load 78-105 (gated at d4d16008, pushed as d8867baf after rebasing over a dots-only commit).
---

Problem: a one-shot hook registered by a persistent hook during IMAGE-LIFECYCLE:PREPARE reserved HOOKS again after it was released; the capture then copied a hook count with no table and the image's own PREPARE threw E-BOUNDS (7122). Acceptance: PREPARE refuses a late one-shot registration with a named error and keeps the registry consistent; the engine carries the fix. Verify: test/image-lifecycle-late-register.f fails on an old engine and passes on the candidate; candidate build via tools/native-build.f.
