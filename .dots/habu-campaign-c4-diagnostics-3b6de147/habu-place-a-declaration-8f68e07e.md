---
title: Place a declaration rejected in evaluated text
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:27:31.766203+01:00"
closed-at: "2026-11-16T04:07:35.111256+01:00"
---

Problem: a declaration run by evaluate prints its record with only a file (test/enum-ctor-collide-bad.f); src/core/sumtype.f TDECL-PLACED-TOK maps no evaluated text. Acceptance: the record stands where the evaluation ran, with its evaluated part. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
