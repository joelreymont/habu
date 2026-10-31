---
title: Build keyed images safely from parallel tests
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T16:39:21.859115+02:00"
---

Six native-file e2e files run in parallel on a fresh tree (.out empty) failed while they first built the candidate's keyed images under .out/<engine key>/: auto and dynamic-load threw -8597 E-NUNIT-PROFILE then -2500 E-PROC-SPAWN, frame threw -3801 E-ENGINE-KEY (own binary cannot be content-hashed); the same run with the images built passed (carl-rcb .out/e2e-*.log vs e2e2/e2e3, lane 11 candidate). Root-cause and fix at the responsible layer. Brief carl-dots/.out/carl-file/brief-race.md. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-race.
