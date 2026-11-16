---
title: "Give PG's throw codes their texts"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T14:50:57.660468+01:00"
---

Problem: src/lib/pg.f 64-74 and src/lib/pg-rows.f 27-29 mint PG's codes as plain constants, so ERROR:TEXT$ is empty for them and Maki's fault answer for a refused JOB-READ (service.f WAIT-STEP, FAULT-ERROR) reads only 'internal_error: code -9321' (tim). Acceptance: each code PG throws is declared by ERROR:CODE or ERROR:FAULT with its text; ERROR:TEXT$ of PG:E-QUERY is that text. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-pg.
