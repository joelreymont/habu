---
title: Normalize orphan dot timestamp
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T22:18:52.988626+02:00"
blocks:
  - habu-orphan-control-word-0370b49d
---

Fresh destruction review of verified-core descendants found commit 78f5e1b1 re-encoded .dots/habu-orphan-control-word-0370b49d.md created-at as an escaped quoted string. Preserve the active status, full claim, description, and implementation outcome; after the orphan-control owner closes/releases the file, restore the canonical single YAML timestamp representation and run the native dot dependency lint. No code changes.
