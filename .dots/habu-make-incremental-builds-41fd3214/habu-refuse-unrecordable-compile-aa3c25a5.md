---
title: Refuse unrecordable compile-time reads
status: open
priority: 1
issue-type: task
created-at: "2026-10-22T17:12:02.971279+03:00"
---

Problem: a file whose compile-time code reads earlier data through a raw address, a TRUSTED: body outside an owner interface, or an untracked foreign or system call cannot list its consumed facts. Acceptance: such a file stores no object and the build names the word (docs/package-build.md last paragraph of Keys and validity); every engine and Dots file the frame census reports ordered=1 (native-file: frame lines from the frame dot, with reason and word) whose compile-time code reads an earlier file's state is converted to a typed read, a dictionary record or a declared input, so no engine file is refused. Verify: an engine build with HABU_NATIVE_CACHE_DIAG=1 shows zero ordered=1 engine files; native-file-frame e2e refusal case. Design: docs/package-build.md "Keys and validity"; working notes ~/.cache/tmp/carl-file/design.md. Depends: frame dot. Claim: unassigned.
