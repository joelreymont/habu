---
title: dot CLI re-quotes created-at on close
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T18:49:20.767636+02:00"
---

Closing a dot rewrites its frontmatter with created-at double-quoted: created-at: '"..."' (escaped quotes nested inside quotes) — see .dots/habu-tfam-5-ordered-4048c839/habu-tfam-5-c-bfa575d2.md and habu-tfam-5-event-d7618516.md after the TFAM-5 redrive close. Cause: the close path re-serializes frontmatter by quoting the already-quoted stored value instead of the raw string. Fix the dot CLI serializer to parse/emit YAML scalars idempotently; add a close-then-reopen round-trip test proving created-at is byte-stable. Repair the two mangled files in the same change.

NOTE (2026-07-04): the dot CLI is an external compiled binary (/opt/homebrew/bin/dot, Mach-O arm64), not repo code — the fix belongs to the dot CLI project, not habu. Kept open as a tracked external defect; the two mangled files in .dots/archive were left as-is (closed, content intact). If the dot CLI source repo is available, fix the frontmatter serializer there (parse/emit YAML scalars idempotently + close/reopen round-trip test).
