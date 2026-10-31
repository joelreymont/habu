---
title: Census trace-only run admissions
status: closed
priority: 2
issue-type: task
created-at: "2026-10-26T12:20:27.635962+02:00"
closed-at: "2026-10-31T20:52:55.344301+02:00"
close-reason: "Obsolete after lane 11: RUN-JUDGE admits only CWORK runs, not by trace; a definition whose compile does work an install would skip compiles live."
---

Problem: RUN-JUDGE (tools/native-unit-dependency.f:1660-1690) admits a top-level seed-word run when its trace changed a record count, here, wordlist count, kernel cell, fact count or output; its comment calls the rule residual: such a run may also change state the object does not carry (the CWORK F9 class). Astra 2026-10-26: the sound rule refuses unaccounted seed runs. Before changing the rule, measure: list every (word, file) a cold engine build and the native-file e2e builds admit by trace alone, and say which carry state outside the file's records. Acceptance: the census table; then a decision dot. Depends: Lay out CWORK from its declarations.

Claim (carl, 2026-10-30): rescoped to runs during a stored definition's compile; worker in .jj-ws.noindex/carl-abandon (brief-runs.md part 2).
