---
title: Keep load order in an all-errors source list
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T05:26:32.079267+02:00"
closed-at: "2026-10-14T19:26:17.643183+03:00"
close-reason: "Implemented in 97ae5543: all-errors source-list replays source segments in load order; reduced require-before-use and dual cases, diagnostic and all-errors suites passed."
---

Problem: tools/check.f --all-errors --source-list still checks whole files in dependency order (CHK-RUN-ALL-LIST-CURRENT in tools/check-core.f calling CHECK-ALL-ERRORS:FILE and SUPPORT+, tools/check-all-errors-core.f:528-541), so a source that defines a word before its require is refused though it loads: the reduced case of habu-expand-a-require-02fabb03 is rc 70 under --all-errors --source-list while plain --all-errors and the default check are rc 0 after that lane (measured by the r4-expand lane). Acceptance: --all-errors --source-list checks each file's segments in load order as the default check does (a require expanded where it sits), the reduced case is rc 0 and its dual still refused E-UNDEFINED with the same location in every mode; the support replay works on spans; cases through tools/check-test-lib.f written before the code. Files: tools/check-core.f, tools/check-all-errors-core.f, tools/check-test-lib.f. Verify: tools/check-test.f, test/gate-diagnostics.f. Depends: habu-expand-a-require-02fabb03. Ownership: all-errors source-list order.
