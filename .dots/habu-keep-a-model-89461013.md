---
title: Keep a model-test stream per worker
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T18:44:36.652338+01:00"
---

Problem: model-test's worker replies borrowed one shared STREAM-OUT (src/lib/net/model-test.f), so a concurrent reply could overwrite another's and the test-model suite failed at random. Acceptance: each worker slot writes its own stream; test-model passes on every run. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-jm-t.
