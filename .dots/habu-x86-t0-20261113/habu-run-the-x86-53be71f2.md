---
title: "Run the x86 callback test's images"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T11:14:00.193482+01:00"
---

Problem: test/x86-64-kernel-callback.f builds a qsort image and four rejection images and runs none of them; RANGE-CASE and BELOW-CASE push a slot callback-entry no longer reads (it answers the shared thunk); the header names 'hb: callback:' lines the kernel no longer writes. Acceptance: each image runs on x86 with its status and record checked, or is deleted with the ffi-callback case that covers it named. Depends: none. Claim: unassigned.
