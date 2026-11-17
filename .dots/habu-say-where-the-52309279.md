---
title: Say where the JSON reader rejected its input
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T17:00:50.959676+01:00"
---

Problem: no JSON reader rejection says where it happened (src/lib/json/read.f NEXT-INNER :682, accessor and STATE sites, copy.f, equal.f, READ-MEMBER$), so an agent cannot fix the input from the text (design /tmp/carl-rplace-report.md, approved). Acceptance: every E-JSON-READ-* and JSON:E-MEMBER rejection that leaves the package carries REASON text 'L:C: expected X, found Y; fix'; json-read-test asserts each. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-j1 (syntax part; the token-place sites follow in a second lane).
