---
title: Ask every COMMIT refusal before publish
status: active
priority: 2
issue-type: task
created-at: "2026-08-03T20:53:12.402056+02:00"
---

Destruction review of NINL, medium. NMIGRATE:WORK runs EMITTED, SIZE-CK, ROOM-CK, REPUBLISH, KEEP-BODY (migrate.f:449-453). ROOM-CK pre-checks capacity so a refusal cannot strand a published word, but COMMIT (inline.f:344-350) has two more refusals — entry 0 <= (E-NINL-STATE) and entry ROW-OF 0 >= (E-NINL-DUP) — that fire AFTER REPUBLISH, leaving the word republished while the migration reports failure. The address is known before publication (A64EMIT:PLACE-AT at migrate.f:433), so all three refusals belong in the pre-check. Also make test/compiler/native-inline.f idempotent: a second RUN in one process throws E-NINL-DUP at :96 because the first run committed a permanent row.

Claim: agent=row-ceiling workspace=.jj-ws/habu-decline-the-row-315c7f64
