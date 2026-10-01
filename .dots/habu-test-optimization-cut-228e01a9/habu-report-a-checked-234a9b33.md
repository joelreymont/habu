---
title: "Report a checked statement's throw in default check mode"
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T10:49:09.873581+02:00"
---

Problem (review 88 on cfdb2bdd): a throw raised while checking a statement is a located record under --all-errors since cfdb2bdd, but default check.f mode still ends 'hb: uncaught throw code N', rc 67, through CHK-PREVERIFY-FAIL (tools/check-core.f:1551, rc CHK-THROW); reduced fixture $HOME/.cache/tmp/kestrel-r4-rev88/scratch/throw-only.f. r4-tbuf (dot 2eb1290e) turns the storage-type refusal class into a checker diagnostic, but any other throw class stays uncaught. Acceptance: default mode (prose and --json-errors) reports a statement's throw through the same located record as --all-errors and exits with the checker's refusal status; a throw class that is not a tbuf refusal is the reduced case, written first and seen to fail. Files: tools/check-core.f, tools/check-test-lib.f. Verify: tools/check-test.f, tools/check-all-errors-test.f, test/gate-diagnostics.f. Ownership: default-mode throw reporting.
