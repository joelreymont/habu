---
title: "Record a left body's facts as they stood at its unseen token"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T19:04:23.311559+01:00"
---

Problem: src/core/check-walk.f:1606 records a body left at an unseen token with facts read after it: `: GEN ( -- ) s" : MADE ( -- ) parse-name drop ; immediate parses: MADE 1" evaluate-closed ;` / `GEN` / `TYPED-VARIABLE KV [ -- ]` / `: LATE ( -- ) MADE parse-name ;` / `: USE ( -- ) [: LATE ;] KV ! ;` loads rc 0 and checks rc 70 E-MISMATCH 5:28. Acceptance: the record holds INHSET as it stood when the body was left, and the check leaves USE to the run. Depends: none. Claim: unassigned.
