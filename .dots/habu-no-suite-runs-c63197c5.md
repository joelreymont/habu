---
title: No suite runs the shadow lint
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T16:21:44.243525+02:00"
---

Problem: tools/lint/shadow-lint.f lists the prefix files and checks each against the primitives, but no suite in test/run.f or test/gate-stdlib-cases.f runs it: it is run only by hand with its header's command. Found by the fold-in worker of habu-an-i64-reaches-d5f2806b; its re-review confirms or corrects. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the lint runs as a suite through bin/hb, fails when a prefix file shadows a primitive (shown by a scratch shadow), and its file list is checked against src/habu/boot-files.f so a new prefix file cannot be missed; or it is deleted if another check already covers it. Files: tools/lint/shadow-lint.f, test/gate-stdlib-cases.f. Verify: the suite row, and the scratch shadow. Depends: none. Ownership: the lints. Claim: unassigned.
