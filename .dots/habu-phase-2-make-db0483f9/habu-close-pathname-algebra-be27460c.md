---
title: Close pathname algebra
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.230361+02:00\""
closed-at: "2026-04-03T23:47:50.739457+02:00"
close-reason: "done: pathname roundtrip, truename/probe-file, directory-only namestrings, and canonical file-op designators verified"
blocks:
  - habu-make-probe-file-e2125665
---

Problem: parse-namestring, merge-pathnames, namestring, and component access are ad hoc string operations. Acceptance: pathname objects preserve roundtrip component fidelity and wildcard semantics. Files: lib/stdlib.habu pathname helpers, src/runtime/primitives/io.zig. Verify: pathname algebra regression set. Blockers: habu-make-probe-file-e2125665.
