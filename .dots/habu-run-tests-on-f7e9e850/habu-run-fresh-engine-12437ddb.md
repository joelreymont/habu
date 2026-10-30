---
title: Run fresh-engine test cases inside their row
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T20:47:21.666421+02:00"
---

Problem: census A (/tmp/jerry-big/inproc-census-a.md table 1, raw /tmp/jerry-big/ca-class.txt) lists test files that start a fresh bin/hb per case with no process boundary under test; first five by cost: test/export-package*.f, test/xt-effect-test.f, test/outer-interpret*.f, test/internal-word-gate.f, test/compiler/native-match*.f. About 110 'test/other' files were classed from their headers only. Acceptance: per file, each case whose subject is not a process boundary runs inside the row (require/evaluate under catch, as test/c2-read-effects.f does); the cases kept as children state their boundary beside them; one lane per few files, listed here before dispatch after the lead reads each file's code. Files: the five named first. Verify: each row green on bin/hb with its child count before and after. Depends: none. Ownership: those files. Claim: unassigned.
