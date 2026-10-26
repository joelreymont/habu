---
title: Admit or explain top-level return stack use
status: active
priority: 1
issue-type: task
created-at: "2026-10-27T00:29:12.734922+02:00"
---

Census B2 (rep/es3.f): '11 22 2>r 2r> . .' at top level is refused 'at 2r>' with no reason under twuqrxuk; master prints 22 11; a body passes. Top-level 'catch' (tier-test ~:888) and 'drop' after USIGS-RESET also refuse without a reason. Ruling: admit what is typeable; every refusal names its reason. Lane: carl-rstk worker.
