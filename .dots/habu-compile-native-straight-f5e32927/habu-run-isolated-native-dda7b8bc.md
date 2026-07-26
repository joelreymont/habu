---
title: Run isolated native objects
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:57:52.852842+02:00"
blocks:
  - habu-lower-native-emission-cbc7f99b
---

Full context: design Wave 2 requires generated candidate bytes to execute outside the publishing engine. Build the minimal validated HBOBJ/object-to-executable boundary and child-process runner for the straight-line slice. Acceptance: output, stderr, exit status, stack result, source map, relocation/object validation, crash, timeout, and malformed-object cases are attributed without publishing a dictionary word.
