---
title: Decide PTX-BARRIER! with no caller outside name-length-test.f
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T01:13:58.634942+02:00"
---

Problem: src/core/checker.f PTX-BARRIER! (the top-level explicit barrier-declaration word) has no caller in src or tools; only test/name-length-test.f names it (:312, :338, :491). A public word with one test caller is an API question for Joel. Acceptance: Joel's ruling recorded here; then the word goes with its test rows, or its purpose is stated beside it. Files: src/core/checker.f, test/name-length-test.f. Verify: rg -n 'PTX-BARRIER!' src tools test. Depends: Joel's ruling. Ownership: src/core/checker.f. Claim: unassigned.

Ruling (Joel 2026-10-30, interview): delete. PTX-BARRIER! goes, with its three rows in test/name-length-test.f (:312, :338, :491). The edit is lane F of habu-remove-the-test-6fc078db.
