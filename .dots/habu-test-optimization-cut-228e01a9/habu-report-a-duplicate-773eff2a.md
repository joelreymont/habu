---
title: Report a duplicate definition under --json-errors
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T11:36:34.855241+02:00"
closed-at: "2026-10-14T19:46:58.930150+03:00"
close-reason: Current CHK-PREVERIFY-DUP renders a duplicate in selected mode; check-test-lib.f asserts JSON and prose with required file and line. Integrated in a5103329 and related duplicate diagnostics commits.
---

Problem: check.f default mode with --json-errors on a duplicate definition exits 78 with empty stderr: CHK-PREVERIFY-FAIL (tools/check-core.f:1800) flushes an empty buffer because the throw classifier excludes DUP-RC (r4-expand commit 8; $HOME/.cache/tmp/kestrel-r4-expand/c8/pre.txt). Acceptance: the duplicate's rename_duplicate record on stderr in --json-errors and the prose line in prose mode, exit 78 kept or changed with the loader agreeing; a case seen to fail first. In a multi-segment session the prose duplicate line names no file (CA-PROSE-DUP, tools/check-all-errors-core.f ~:250-252, review 143), so only the JSON record says which file defined it twice: the prose line names the file as the record does, in a case with the duplicate in a required file. Files: tools/check-core.f, tools/check-all-errors-core.f.
