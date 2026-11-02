---
title: "lsp-entry-test's 30 s child deadline fails under load"
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T10:05:54.419347+02:00"
---

Problem: the batch-2 full suite (2026-11-03, carl-b2 hb, load average 138 on 12 cores while two other suites and a two-generation build ran) went red on one row: lsp-boundary, where tools/lsp-entry-test.f, after tools/lsp-test.f's 'test: ok', ended with uncaught -2502 (E-PROC-TIMEOUT), rc 67. SERVER-RUN and CHILD-RUN (:117, :144) give each child 'ENGINE-CANDIDATE:PATH$ 30000 >MS CMD:RUN-RC', and the first child builds a cold image (STAGE's HABU_OUT). Alone, at load 39-58, the file passes in 13-15 s for all four children on both the installed hb and the batch-2 hb, so the change did not slow it. A fixed wall-clock deadline sized for an idle machine is a test that fails by load. Acceptance: decide what the deadline guards (a hung child), and make the row pass under the suite's real load without losing that guard: e.g. the suite row's own timeout as the only bound, or a deadline that is not wall-clock-on-an-idle-machine; state the choice in the file's comment. Files: tools/lsp-entry-test.f (and any other test with the same pattern found by rg '>MS CMD:RUN-RC' in tools/ test/ if the same reasoning applies; list them, do not widen silently). Verify: the lsp-boundary row in a full suite run. Depends: none. Ownership: tools/lsp-entry-test.f. Claim: unassigned.
