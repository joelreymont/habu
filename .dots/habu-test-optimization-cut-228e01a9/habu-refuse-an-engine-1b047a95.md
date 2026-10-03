---
title: Refuse an engine-provided all-errors subject
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T07:11:23.848135+02:00"
---

Problem: tools/check.f --all-errors on one file the engine already provides (lib/string.f) has no segments, so it checks nothing; with --json-errors it prints the run's prose 'duplicate definition: STR-TAB at .../run.f:21' instead of a JSON record (rc 78), while --source-list refuses all-provided inputs in CHK-CHECK-LIST-INPUTS (tools/check-core.f:806) and single-file mode (CHK-MATERIALIZE-FILE, :789) does not (found by the r4-expand3 lane on uuykplso e81a91a0). Acceptance: single-file and list modes give the same located refusal for an engine-provided subject, as a JSON record under --json-errors and prose otherwise, through one check; cases in tools/check-test-lib.f written first and seen to fail. Files: tools/check-core.f, tools/check-test-lib.f, docs/repair-diagnostics.md if the record is new. Verify: tools/check-test.f, tools/check-all-errors-test.f, test/gate-diagnostics.f. Ownership: all-errors subject admission.
