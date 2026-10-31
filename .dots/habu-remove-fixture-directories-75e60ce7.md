---
title: Remove fixture directories tests leave under TMPDIR
status: open
priority: 3
issue-type: task
created-at: "2026-10-31T19:25:44.823620+02:00"
---

stuart observed (2026-10-31) that src/lib/jsonl-test.f, tools/lsp-test.f and tools/lsp-entry-test.f leave their fixture directories under TMPDIR after a passing run. Each test removes what it creates. Ownership: carl. Claim: unassigned.
