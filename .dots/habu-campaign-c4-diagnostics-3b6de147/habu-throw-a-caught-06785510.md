---
title: Throw a caught code again in the unit that caught it
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T11:12:31.118361+01:00"
closed-at: "2026-11-18T11:29:27.472034+01:00"
---

Problem: a code caught in one word and thrown again by another is a new throw, so the record it carried is dropped (src/habu/t0.f:1723-1733): src/core/generated-declaration.f GUARD-CODE throws what DRIVE caught, src/core/generated-declaration-protection.f FAIL throws what RUN caught, and src/habu/verify-source.f IDENTITY-OUTCOME, LEGACY-TYPE-OUTCOME, STORAGE-OUTCOME and VALUE-RECORD-OUTCOME throw what their callers caught. A refused map inside a declaration or a verify-only record loses its bytes and help. Acceptance: each of these throws is in its catch's unit, as src/habu/t0.f COMPILE does after SETTLE-UNIT. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-mm0a and .jj-ws.noindex/jerry-mm0b.
