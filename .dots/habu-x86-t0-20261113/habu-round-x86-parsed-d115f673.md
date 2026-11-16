---
title: Round x86 parsed floats once
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:44:56.638284+01:00"
---

Problem: x86 NUM-PARSE, rounds a decimal float two or three times (9.9996 one ulp high). Acceptance: interpreter-number passes on x86. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-x86 (merged, lands with the feature).
