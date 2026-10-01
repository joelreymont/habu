---
title: Locate lexer defects in default check mode
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T11:36:34.841716+02:00"
---

Problem: in check.f's default mode an open PRIM: row and an unterminated string end in a prose die with rc 74 from src/habu/verify-source.f:1040 and :253, with no location and no record, while --all-errors reports close_primitive_row and close_string records (r4-expand commit 8; $HOME/.cache/tmp/kestrel-r4-expand/c8/pre.txt). Acceptance: default mode, prose and --json-errors, gives the same located records and rc 70; the loader's own message unchanged; cases seen to fail first; engine rebuilt (verify-source.f is baked). Files: src/habu/verify-source.f, tools/check-core.f.
