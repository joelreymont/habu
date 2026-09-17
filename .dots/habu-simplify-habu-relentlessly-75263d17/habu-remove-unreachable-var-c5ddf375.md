---
title: Remove unreachable variable axiom verification
status: closed
priority: 1
issue-type: task
created-at: "\\\"2026-08-23T18:41:30.073373+02:00\\\""
closed-at: "2026-09-16T14:34:50.600613+03:00"
close-reason: "superseded by habu-campaign-c2-mem-c3d7662b: Still true: the variable axiom and its metadata-only verification remain in the checker model"
---

Make production verification reject parser-only variable/constant tokens inside bodies, delete the unreachable variable axiom and metadata-only tests, and retain only the load-bearing constant boot behavior until its hook protocol is simplified.
