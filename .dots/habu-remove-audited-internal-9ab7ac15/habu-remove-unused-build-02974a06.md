---
title: Remove unused build step records
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:47:04.240152+03:00"
closed-at: "2026-10-16T11:55:28.796724+03:00"
close-reason: Deleted unused build step records/state/setters and exclusive tests/docs; preserved simple CHECK, ARTIFACT, STEP and RUN utilities. Build helper tests and hb-build/build-fixpoint/hb-build-test library loads pass.
---

BUILD step records and lifecycle setters have no production consumer; argv is metadata never executed and tmp is only validated. Delete the unused record/state framework and exclusive tests/docs, retaining the simple CHECK, ARTIFACT, STEP and RUN utilities. Confirm callers and run surviving build helper tests and requiring tool smoke checks.
