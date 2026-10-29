---
title: Cut comments in test
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T21:49:33.460472+02:00"
---

Problem: the comments under test restate code, narrate history or name deleted words; rule and outcomes in the parent and /tmp/jerry-big/t0-comments-brief.md. Acceptance: one comment-only commit for test (code lines byte-identical), every kept comment a fact the code cannot state, headers a short map, no named word missing from the tree; reviewed by a code-reviewer against the rule and landed. Files: test. Verify: the comment-only diff filter prints nothing; the names check finds every named word; hb --load tools/check.f on each file where the engine does not provide it. Depends: none (the worker takes the directories in the parent's order). Ownership: comments under test. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0c-test.

2026-10-30: the sweeps and the 462 files with 1-10 comment lines done in susunvym (jerry-t0c-test, 113 files edited); the rest split by comment count: 11-30 lines (153 files) stay in susunvym, 31-100 (150 files) in mvomzvvp (jerry-t0c-test2), over 100 split in two by lines: 51 files in vyowuzvy (jerry-t0c-test3), the 18 largest in kqwpvulv (jerry-t0c-test4); squashed into susunvym before review. Workers relaunched 00:30.

2026-10-30 01:25: split further: jerry-t0c-test5 (files 89-150 of part 2's list) and jerry-t0c-test6 (files 38-51 of part 3's list), both on 5505243f; the main commit's edits are complete and its check.f batch runs in the background (.out/check30-summary.txt); parts 2, 3, 4, 5 and 6 running. test/gate-pool.f, gate-pool-test.f and gate-signal-test.f are left to the timer lanes; test/suite-budget.f's comment edit yields to lane 2's deletion at landing.
