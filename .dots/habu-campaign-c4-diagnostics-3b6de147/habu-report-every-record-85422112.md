---
title: Report every record before an all-errors lexer defect
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T20:50:18.896608+01:00"
closed-at: "2026-11-16T04:07:34.242994+01:00"
---

Problem: check.f --all-errors on a file with an undefined word on line 1 and an unclosed string on line 2 prints only the lexer's record: discovery's walk (tools/check-core.f CHK-MATERIALIZE, tools/source-discovery.f SD-UNTERM-STOP) writes that record and stops the check before the composition runs, and tools/check-all-errors-core.f CA-CHECK-COMPOSE-VERIFY composes without the quiet mode that rejects discovery's forms where it meets them, so a retire or shadow form has no record there. Acceptance: that file gives line 1's record, then line 2's, rc 70; every record once, in load order. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
