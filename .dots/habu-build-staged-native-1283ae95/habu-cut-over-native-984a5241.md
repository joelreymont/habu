---
title: Cut over native objects
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:56:29.082266+02:00"
blocks:
  - habu-retire-native-byte-f44351f7
---

Full context: design Wave 7 introduces canonical HBOBJ, explicit symbols/relocations/source maps, linking, AOT tree shaking, target object consumption, and complete cache keys. Acceptance: AOT never scans BL encodings; current AOT/REPL build gates pass from structured objects.
