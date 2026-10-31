---
title: "Place a refused does> clause's ncomp line"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T21:25:14.228941+02:00"
---

Problem: when the native compiler refuses a `does>` clause, its `ncomp: cannot compile <definer> at <token>` line names no file or line. /tmp/jerry-dtailrr2-tmp/p/mkl.f on lane dtail's candidate prints `ncomp: cannot compile MKL at n<newline>` with no ` in <file>:<line>`. Lane sealed adds the place to `ncomp:` lines through definition.f SAY-WHERE (`ncomp: cannot compile RV-X at drop in .../sw1.f`); check after dtail and sealed land whether a clause refusal gets it, since the clause's signature stands only in the dictionary copy (habu2.f C-PARSE-CREATED-SIG), not in the scanned text. Acceptance: a refused clause's `ncomp:` line ends ` in <file>:<line>` with the clause's line under the place rule (the file's line for the file's own text, the file alone for an evaluated string), at tier 0 and tier 1; a row in test/certify-does-definer.f asserts it. Files: src/compiler/native/definition.f (SAY-WHERE, CHECKER-REFUSED$), test/certify-does-definer.f. Verify: mkl.f, mkx-json.f on the candidate; row certify-does-definer. Depends: lanes dtail and sealed landed (habu-check-a-does-6213c5c4, habu-answer-tools-check-745b0ccd). Claim: unassigned.
