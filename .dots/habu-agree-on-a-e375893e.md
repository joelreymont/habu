---
title: "Agree on a top-level {: between load and check"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T07:07:10.312365+01:00"
---

Problem: a top-level `{:` gives E-UNDEFINED from the load but E-UNCLOSED-LOCALS from tools/check.f, on the installed hb: two writers judge one token. Acceptance: the load and check.f print the same record for a top-level `{:` (closed and unclosed), written by one word; a test pins it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w196.
