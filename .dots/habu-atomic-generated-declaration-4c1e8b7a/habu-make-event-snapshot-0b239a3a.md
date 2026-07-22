---
title: Make event snapshot failure atomic
status: closed
priority: 1
issue-type: task
created-at: "2026-07-22T22:24:41.185704+02:00"
closed-at: "2026-07-22T23:31:15.008623+02:00"
close-reason: landed as d68787a8; fresh destruction READY; focused suites, candidate validation, exact diff lints, Maki, PTX stdlib, host-lint, filemap-lint, and dot gate green on master e446a273
---

Destruction review found that DECL-EVENT:DEV-OPEN changes the serial, field base, field and variant ordinals, and current selector before the fallible PF-BEGIN call. When the field owner rejects snapshot creation, no declaration-event frame exists and coordinator rollback cannot restore those cells. On base `674e2d49b891`, the real `GENERATED-DECL:RUN` path with `PF-TX-SERIAL` at the maximum signed value returns `E-PF-TX` 7123 but changes the event serial, field base, field and variant ordinals, and current selector. Existing focused suites remain green.

Ownership is the private `DECL-EVENT` snapshot adapter and existing TYPE-FIELD transaction owner. Keep the existing transaction and frame format. Reorder `DEV-OPEN` exactly:

1. Ensure the event-frame arena has capacity.
2. Compute and validate the next event serial without storing it.
3. Call `DEV-FLD-BEGIN`.
4. Store the validated event serial, outer baselines, and complete event frame including the returned field token, then increment `DEV-TX-DEPTH`.

Everything after `DEV-FLD-BEGIN` is preallocated primitive reads, stores, and arithmetic and therefore cannot throw. A failed field begin changes neither transaction depth. A successful `DEV-OPEN` advances both depths before returning. `SNAPSHOT-ALL` includes this participant in rollback, so any later participant failure reaches `DEV-ROLLBACK` and the recorded field token. Do not add a production API, owner query, baseline format, savepoint, rollback path, selected-cell repair guard, copied event model, or public failure hook. Preserve monotonic non-reused tokens, nested last-in-first-out behavior, and the original failure code.

Red proof belongs in `test/decl-event-suite.f` and must run through `GENERATED-DECL:RUN`. Use one private, test-only `TRUSTED` swap of `PF-TX-SERIAL` to the maximum signed value, recorded in `TRUSTED.md` under the standing atomic transaction dot. Assert exact `E-PF-TX` and exact equality of `DEV-N`, `DEV-PUB-N`, `DEV-BASE-FLD`, `DEV-FLD-ORD`, `DEV-VAR-ORD`, `DEV-CUR-VAR`, `DEV-TX-DEPTH`, `DEV-TX-SERIAL`, provisional and committed field counts, `PF-TX-DEPTH`, and `PF-TX-SERIAL`. Restore the test-only swap only after those assertions, then prove failure followed by success matches a clean success. The existing candidate-validation row already enrolls the suite; add no suite or inventory row.

Acceptance: the focused production-path regression, later-participant rollback, nested success and rollback, declaration-event, generated transaction, STRUCTURE, ENUM, rollback, snapshot, AOT, exact-diff typed-local and package lints, candidate validation, and native fixpoint are green. Files: `src/core/decl-event.f`, `test/decl-event-suite.f`, and `TRUSTED.md` only.

Claim: agent=event_snapshot_impl workspace=.jj-ws/habu-make-event-snapshot-0b239a3a
