---
title: Report, never throw, after a refused segment
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T05:41:14.516787+02:00"
---

Problem: tools/check.f --all-errors --source-list on lib/net/http-response.f (and http-router.f, http-static.f, which load it) exits rc 67 with 'hb: uncaught throw code 7121' (E-SIZE / E-LAYOUT-BUFFER) once the refused http-request.f leaves TCP4:connection undefined: a later segment's layout or size query throws past the checker instead of becoming a diagnostic (measured by the r4-expand lane; the whole-file check in b176b974 does the same). Acceptance: a throw raised while checking a segment, including layout and size queries on a type an earlier refusal left undefined, is reported as a located diagnostic and the run ends with the checker's own exit status, never an uncaught throw; the three files end rc 70 with their real first refusal; cases through tools/check-test-lib.f written before the code. Files: tools/check-core.f, tools/check-all-errors-core.f, tools/check-test-lib.f. Verify: tools/check-test.f, tools/check-all-errors-test.f, test/gate-diagnostics.f. Depends: habu-keep-load-order-75f62e37. Ownership: all-errors segment throw handling.
