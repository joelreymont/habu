---
title: Bound the test rows' wall deadlines by CPU
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T11:39:18.138408+02:00"
---

Problem: in the full gate at load average ~90, lsp-boundary and prefix-provided-unfilled report TIMEOUT-UNDER-LOAD and pass alone (prefix-provided-unfilled in 334 s). Rows are CPU-budgeted (test/suite-budget.f, closed dot habu-bound-long-suite-6d93453d), but these rows keep inner wall deadlines: tools/lsp-test-lib.f:620-621 CONVERSATION-MS 8 s and CHECKED-MS 60 s, test/prefix-provided-unfilled.f:29,88 BUILD-TIMEOUT-MS 600 s. Acceptance: each inner deadline is bounded by CPU use or by the row's hang guard, so load alone cannot fail the row; a saturated gate runs both rows to PASS. Files: tools/lsp-test-lib.f, test/prefix-provided-unfilled.f, test/suite-budget.f. Verify: the two rows under a saturated host. Depends: none. Ownership: test gate. Claim: unassigned.

More evidence (tim, FD lane gate, load 103-193): tools/lsp-entry-test.f throws -2502 when its children hit the fixed 30000 ms CMD:RUN-RC limit (near :127 and :149); it passes at load ~110 and fails at ~190, on two different engines at the same moment and on another base (tim-linvar). prefix-provided-unfilled hit the pool's 600 s row limit and passes alone in 982 s wall at ~29% CPU, mostly the linux-x86-64 cross build. Add tools/lsp-entry-test.f to the files.

More evidence (tim, stdio re-review, master's engine and the candidate at the same moment, same 22.8 s user CPU each): (1) check-drivers' case `tools/check.f -- tools/build-profile.f` runs past check.f's own fixed wall deadline CHK-DEADLINE-MS 120000 (tools/check-core.f:66, CHK-RUN-LATE :1562-1569) and exits 70 where the row expects 67, so the suite exits 1 and the pool reports kind=exit, not a timeout; master's engine gave 70 at 136 s, the candidate 67 at 122 s. (2) check-test's file-load-context case fails CHILD-HANG-MS, "child never exited; deadlock guard ms: 54000" (tools/check-test-lib.f:53-73: WORST-CHILD-MS times HANG-MARGIN 4 is a wall budget). Add tools/check-core.f and tools/check-test-lib.f to the files; both deadlines are wall clocks that load alone can exhaust. Logs: .jj-ws.noindex/tim-stdio/.out/rereview-stdio/runs/.

More evidence (tim, Maki canon-ops/.out/fold3/): test/db/pg-cluster.f gives each case a wall deadline, DEFAULT-CASE-MS 60000 (:98) unless HB_PG_CASE_MS overrides it (:165-168). Under load Maki's `pg-cluster.f -- test/chat-case.f` hits it at about 64 s (rc 67) and passes in 226 s with HB_PG_CASE_MS=300000. Add test/db/pg-cluster.f to the files; the same CPU bound applies to its case and step deadlines.

Note 2026-11-03: test/suite-budget.f, cited above, no longer exists. tools/lsp-entry-test.f's two fixed 30000 ms limits go in "Let the LSP entry test wait without a clock" (dots habu-lsp-entry-test-4d67fa4a, habu-make-lsp-entry-c1c4a339); the other deadlines here stay open.

Note 2026-11-05: test/prefix-provided-unfilled.f is deleted, its BUILD-TIMEOUT-MS with it: two whole x86-64 engine builds (578 s, the suite's second slowest test) checked one refusal of the x86-64 capture build, and its second build reused no file object (dot habu-fit-cache-diagnostics-ef6f5f55: reused 0, compiled 176).

Recurred (2026-11-06, jerry): the A1+B+A2a landing suite (gate engine /tmp/jerry-gate/gen2, load 11 rising to 50, 545 s wall) failed only check-cli-boundary, "check/file-load-context - child never exited; deadlock guard ms: 54000"; the same test alone on the same engine and tree passed in 354 s wall, 124 s user. One trivial `bin/hb --load tools/check.f -- ok.f` child takes 1.5 s user and 4 s wall at load 24, against the 0.62 s heaviest child tools/check-test-lib.f:51-55 states, so the guard's margin is a third of what its comment claims. Which child expired is not known: the suite removed its temporary directory.

Consumers: Maki's chat-case is gone, so no consumer meets this today; the deadlines of Habu's own rows stay as described.
