---
title: Stop file-object hits throwing in stripped builds
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T23:07:29.609463+03:00"
---

Join stack xqznxmkk: stripped/AOT builds throw E-NUNIT-PROFILE (-8597) on a file-object hit (~24 RED gate rows: stripped-*, hb-build-stripped*, hb-build-aot-*, native-file-*). Reproducer: test/stripped-entry.f with a fresh cache on the stack engine. Lane: worker in .jj-ws.noindex/carl-hit (@ = xqznxmkk + urspuwsy); brief /Users/joel/.cache/tmp/carl-file/brief-hit.md; results /Users/joel/.cache/tmp/carl-hit/. Gate evidence: /Users/joel/.cache/tmp/carl-jgate.v29Z/suite.log.
