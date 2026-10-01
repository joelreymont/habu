---
title: Report an unterminated string as a JSON record
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T11:36:34.823352+02:00"
---

Problem: bin/hb --load tools/check.f -- --json-errors <file> (with or without --all-errors) on a file with an unterminated string prints the prose line 'check.f: discovery rejected: unterminated string', rc 70, not a record (tools/check-core.f:908, raised at :912); the same source on stdin under --all-errors gives the close_string record r4-expand commit 8 defined. Found by r4-expand commit 8 (pre-fix run $HOME/.cache/tmp/kestrel-r4-expand/c8/pre.txt). Acceptance: every --json-errors mode emits the close_string span record for a file; diag-contract and repair-packet accept it; a case seen to fail first. Files: tools/check-core.f, tools/check-test-lib.f.
