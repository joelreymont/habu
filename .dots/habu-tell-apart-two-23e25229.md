---
title: Tell apart two quotation types that print alike
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T13:16:26.535443+01:00"
---

Problem: ': G ( R [ R -- R ] -- R ) dup execute ;' is rejected with "`execute` takes nothing; the stack holds [ -- ]": an occurs failure in src/core/checker.f U-ROW sets UOK false without U-FAIL, so no failing pair or reason is recorded and the rendering trims the rows until expected and found print alike. Acceptance: an occurs failure's rejection names the row variable and the row that would contain it, so expected and found differ; a test pins G and fails first. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-alike.
