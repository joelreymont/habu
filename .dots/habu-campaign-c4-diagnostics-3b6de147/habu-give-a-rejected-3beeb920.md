---
title: Give a rejected engine selection its record
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:34:21.157266+01:00"
closed-at: "2026-11-16T04:07:36.076875+01:00"
---

Problem: tools/check-core.f: under --json-errors a rejected engine selection (HABU_UNDER_TEST not an executable) prints a prose line on stdout; without it the generic E-THROW -2102 record loses what was selected. Acceptance: one record naming the selection and how to fix it, prose or JSON as asked, on stderr. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
