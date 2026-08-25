---
title: Split cost out of the committed corpus tables
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T22:13:34.881037+02:00"
---

--update corpus4 rewrites the COST column wholesale with the updating host's timings (5-25% off committed on a loaded host) while bytes/outputs are stable — a byte-motivated re-pin silently re-pins every timing. Split cost into its own file or tag the host/date per the harness's own honesty discipline, so byte re-pins stop carrying timing churn. Found 2026-08-07; sibling concern: codegen-spill-probe sits in the stdlib gate while its refusal cases sit in test/run.f — one compiler change reds two gates; co-locate.
