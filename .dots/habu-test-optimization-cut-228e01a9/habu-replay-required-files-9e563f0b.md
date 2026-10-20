---
title: Replay required files in a plain all-errors check
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T05:41:14.508015+02:00"
closed-at: "2026-10-14T19:26:17.822405+03:00"
close-reason: "Implemented in 7f4f38b3: plain all-errors uses the same expanded segment replay as source-list; required-word and real-undefined cases, focused suites and diagnostics passed."
---

Problem: tools/check.f --all-errors on a single file replays none of the files the subject requires, so a subject that uses a word from a require the engine does not already hold is refused: scratch al/main.f is rc 70 under --all-errors and rc 0 by default and under --all-errors --source-list (measured by the r4-expand lane on b176b974 and on the base). Acceptance: plain --all-errors installs the same segment replay the source-list mode uses, so every mode gives a subject the verdict its real load path gives; the al/main.f shape checks rc 0 in all three modes and a real undefined word is still refused at the same location in each; cases through tools/check-test-lib.f written before the code. Files: tools/check-core.f, tools/check-test-lib.f. Verify: tools/check-test.f, tools/check-all-errors-test.f, test/gate-diagnostics.f. Depends: habu-keep-load-order-75f62e37. Ownership: plain all-errors replay.
