---
title: "Hold one declaration's event state without tokens"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T14:41:54.692827+01:00"
closed-at: "2026-11-16T04:07:35.814533+01:00"
---

Problem: DECL-EVENT (src/core/decl-event.f) mints a token per frame and keeps a DEVTX frame stack (DEV-OPEN, DEV-TX-TOP, DEV-TX-REQUIRE, DEV-TX-OPEN-REQUIRE, E-DEV-TX), but DEV-OPEN's one caller is PART-SNAPSHOT, run once per declaration under DECLARATION-TRANSACTION:RUN, whose OPEN rejects a second declaration (E-ACTIVE): the depth never exceeds 1, and every E-DEV-TX throw marks only an engine defect. The comment above DEV-PART-FIELD-REQUIRE claims a negative fixture no test holds. Acceptance: one declaration's event state is held directly, with no token, frame stack or E-DEV-TX; the declaration suites pass. Depends: habu-merge-master-s-b11a4824. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
