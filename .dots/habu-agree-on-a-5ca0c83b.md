---
title: "Agree on a private primitive's record"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T06:18:08.211251+01:00"
---

Problem: a top-level call or tick of an internal engine primitive outside the packages its private rows belong to, '0 0 0 xref-retarget' or "' xref-retarget drop", gets E-UNSAFE 'internal to the engine' from the load (src/habu/interpreter.f XT-GATE, DNAME-INT) but E-UNDEFINED from tools/check.f; docs/forth-card.md section 3 states E-UNDEFINED for such a call or tick. Two writers judge one token. Acceptance: the load and check.f print the same record for that file and for the same call in a body, from one word that writes it; the card row states that record. Depends: none. Claim: unassigned.
