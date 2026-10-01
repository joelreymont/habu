---
title: Check tic6x sim.f and http libs as they load
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T04:19:38.371221+02:00\""
closed-at: "2026-10-01T17:13:05.095822+02:00"
close-reason: Fixed by e9e457a3 (review 63 ACCEPT)
---

Problem: tools/check.f refuses sources that load and pass their own tests: src/arch/tic6x/sim.f stops "bad nominal type 'SIDE'" rc 70, and lib/net/http-request.f, http-router.f and http-static.f stop throw 7121 rc 67 (measured on d40cc36d by the r4-loop lane, identical on its engine). The http ones may be the computed-buffer-count case that change pzrymmlt and plan 15 (dot f02aa703) fix; re-measure on that result first. Acceptance: on the r4-render result each of the four checks with the verdict its load path gives, or the remaining refusals are root-caused (checker, declaration or source) and fixed at that layer; a case through tools/check-test-lib.f for each distinct cause, seen to fail first. Files: as the cause dictates. Verify: tools/check-test.f, tools/check.f on the four files. Depends: habu-let-check-f-f02aa703. Ownership: check.f verdicts on these sources.
