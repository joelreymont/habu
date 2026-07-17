---
title: Repair publish trust effects
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-17T14:54:50.289053+02:00\""
---

Full gate on sol-change-file-v2 fails trust-lint: src/habu/habu2.f EM-COMPILE-PUBLISH-TRUSTED code effect is label -- but TRUSTED.md records --; EM-COMPILE-PUBLISH-HOOKED code effect is label label -- but manifest records --. Update the exact manifest rows to the implemented checked effects, run trust-lint and full gate. Dependency: already-landed publish-tail sharing commits.
