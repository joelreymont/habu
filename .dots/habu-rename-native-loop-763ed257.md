---
title: "Rename native-loop.f's corpus-row labels"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T01:13:58.620476+02:00"
---

Problem: test/compiler/native-loop.f T-LABEL strings say 'the corpus row'; no corpus harness exists in the tree (the row is NLPT-WIDE). Acceptance: the labels name the row. Files: test/compiler/native-loop.f. Verify: rg -n corpus test/compiler/native-loop.f prints nothing; bin/hb --load test/compiler/native-loop.f rc 0. Depends: none. Ownership: test/compiler/native-loop.f. Claim: unassigned.
