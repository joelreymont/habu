---
title: Census trace-only run admissions
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T12:20:27.635962+02:00"
---

Problem: RUN-JUDGE (tools/native-unit-dependency.f:1660-1690) admits a top-level seed-word run when its trace changed a record count, here, wordlist count, kernel cell, fact count or output; its comment calls the rule residual: such a run may also change state the object does not carry (the CWORK F9 class). Astra 2026-10-26: the sound rule refuses unaccounted seed runs. Before changing the rule, measure: list every (word, file) a cold engine build and the native-file e2e builds admit by trace alone, and say which carry state outside the file's records. Acceptance: the census table; then a decision dot. Depends: Lay out CWORK from its declarations. Claim: unassigned.
