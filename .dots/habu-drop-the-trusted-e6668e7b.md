---
title: "Drop the TRUSTED shims around CHECKER-BOUND:REWIND"
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T14:18:09.017415+03:00"
---

Problem: test/defer-history-child.f and test/compiler/native-prefix-rollback.f each wrap CHECKER-BOUND:REWIND in a TRUSTED: shim; the checker row habu-give-the-build-4b825045 added makes the word callable from checked code, so the shims are two unchecked seams that state nothing. Acceptance: both files call CHECKER-BOUND:REWIND from plain colon definitions, no TRUSTED: remains in either for that purpose, both tests green on an engine built after 4b825045. Files: test/defer-history-child.f, test/compiler/native-prefix-rollback.f. Verify: the two tests through bin/hb. Depends: habu-give-the-build-4b825045. Ownership: tests. Claim: unassigned.
