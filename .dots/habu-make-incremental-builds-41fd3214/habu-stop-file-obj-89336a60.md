---
title: Stop file-object hits throwing in stripped builds
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T23:07:29.609463+03:00"
---

Join stack xqznxmkk: stripped/AOT builds throw E-NUNIT-PROFILE (-8597) on a file-object hit (~24 RED gate rows: stripped-*, hb-build-stripped*, hb-build-aot-*, native-file-*). Reproducer: test/stripped-entry.f with a fresh cache on the stack engine. Lane: worker in .jj-ws.noindex/carl-hit (@ = xqznxmkk + urspuwsy); brief /Users/joel/.cache/tmp/carl-file/brief-hit.md; results /Users/joel/.cache/tmp/carl-hit/. Gate evidence: /Users/joel/.cache/tmp/carl-jgate.v29Z/suite.log.

Likely cause (L2a hand-off 3, relayed to the worker): the does> re-publish rebinds the record in RETIRE-REUSED (tools/native-file-reference.f:665-672) and uses up a name occurrence. Ruling: refresh the token on a same-declaration re-stamp, never replay a second publication at install.
