---
title: Delegate composite-stream char ops for file-driven query I/O
status: active
priority: 1
issue-type: task
created-at: "\"2026-03-07T19:20:07.522966+01:00\""
blocks:
  - habu-load-testsuite-generr-7386a168
---

src/runtime/primitives/io.zig:1748-1888; lib/stdlib.habu:7183-7201; ../maxima/src/mload.lisp:379-509. Root cause: readChar/unreadChar/peekChar/listen return NotImplemented on echo/two-way/synonym/concatenated streams. Fix: delegate through underlying input streams for the query-IO path. Why: canonical test-batch with answers_from_file=t cannot work otherwise.
