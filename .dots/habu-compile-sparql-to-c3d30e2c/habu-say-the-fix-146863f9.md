---
title: Say the fix in SPARQL rejections
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T10:43:28.497423+01:00"
---

Problem: src/lib/sparql.f:6 promises rejections as `L:C: expected X, found Y; FIX`, but the compiler answers without the fix (W3C syntax-general-04: `2:25: expected a construct this compiler handles, found `11``); Habu's rejections tell the agent how to fix the query. Acceptance: every rejection the parser and compiler raise ends in a fix; sparql-test.f asserts the full text of one per rejection site class. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-sparql (merged into the line).
