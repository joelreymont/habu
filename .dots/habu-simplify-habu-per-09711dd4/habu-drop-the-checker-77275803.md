---
title: Drop the checker HIDX epoch memo
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.825984+03:00"
---

Problem: the HIDX-EFF@/HIDX-CTL@ epoch memo sits in front of the exact O(1) USX and NRX indexes (CHECKER-FIND-USIG-SYM checker.f:13045, CTL-WORD-SYM :15566). The memo header at 7081-7087 predates USX. About -70 lines. Acceptance: these lookups go straight to USX/NRX; the memo storage and its epoch bumps are deleted; checker diagnostics are unchanged. Files: src/core/checker.f. Verify: rebuild bin/hb (checker is in the engine prefix); checker suites named by test/gate-stdlib-cases.f; bin/hb --load test/run.f (shared checker behavior). Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. Ownership: src/core/checker.f memo section. Claim: unassigned.
