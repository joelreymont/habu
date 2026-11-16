---
title: "Agree on a private primitive's record"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T06:18:08.211251+01:00"
---

Problem: a top-level call of a private primitive outside its package, '0 0 0 xref-retarget' (/tmp/jerry-w188/v/xr.f), gets E-UNSAFE 'internal to the engine' with help 'call a public word instead' from the load, but E-UNDEFINED 'not found' from tools/check.f, on the installed hb; docs/forth-card.md:224 states E-UNDEFINED. Two writers judge one token. Acceptance: the load and check.f print the same record for that file and for the same call in a body, from one word that writes it; the card row states that record. Depends: none. Claim: unassigned.
