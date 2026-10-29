---
title: Build a Linux arm64 product engine from current source
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T20:53:13.790758+02:00"
---

No route builds one today. Cross-build from the Mac stops at native-build-core.f OUTPUT-LAUNCH-CK (rc 74, by design); spark's installed Habu is stale (E-A-FIRST); tools/bootstrap.sh on spark (gforth 0.7.9) builds a recovery engine whose hb-build --engine fails rc 70, E-UNDEFINED NATIVE-RUNTIME:CAPTURE-PREPARE in save. W3L's snapshot-writer, debugger-resume and self-rebuild rows cannot run on Linux until this is fixed. Root-cause lane in .jj-ws.noindex/carl-linux, brief .out/brief-linux-build.md; proofs on spark.
