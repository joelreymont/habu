---
title: Enforce one FILEMAP entry per line
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T09:01:14.723962+02:00"
---

Problem: tools/filemap-lint.f matches tracked paths anywhere in a line, so it accepted a structurally corrupted FILEMAP.md in which two entries were concatenated on one line and one entry was thereby effectively deleted - found during the replay-registration review. Required result: the lint enforces the one-entry-per-line shape structurally: each entry line contains exactly one path in the entry position with its description, and a line containing two path tokens, a path outside the entry position, or a malformed entry shape is a finding. Hostile fixtures per the test-integrity rule: concatenated double entry, path inside a description, path inside a comment, duplicate entries, reordered sections, wrong-role lines. Acceptance: the corrupted two-entries-one-line shape reds the lint; the clean FILEMAP.md stays green; mutations disabling the shape check fail the focused test. Files: tools/filemap-lint.f, tools/filemap-lint-test.f. Verify: bin/hb --load tools/filemap-lint-test.f and the repository filemap-lint gate. Depends: none. Ownership: filemap lint line-shape rule only. Claim: unassigned.
