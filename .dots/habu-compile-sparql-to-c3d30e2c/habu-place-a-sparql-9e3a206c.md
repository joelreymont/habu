---
title: Place a SPARQL escape rejection where written
status: closed
priority: 2
issue-type: task
created-at: "2026-11-27T21:04:10.816716+01:00"
closed-at: "2026-11-27T22:36:33.196787+01:00"
---

Problem: src/lib/sparql/lex.f DECODE-ESCAPES pushes its offset pairs into SYNTAX-ESCAPES while it still scans the written text, so a rejection in that pre-pass is placed by WRITTEN (syntax.f) as if its offset were decoded: each earlier \u shifts the place 5 columns, \U 9, up to a line:column past the query. Joel: a rejection prints its place. Acceptance: every pre-pass rejection is placed at its escape in the written query; a test has a bad escape after a good one. Depends: none.
