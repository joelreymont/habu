---
title: Retire pointer-term arenas at file boundaries
status: active
priority: 1
issue-type: task
created-at: "2026-10-26T13:24:06.648344+02:00"
---

First earlier-write site PTRA-BOOT+8 (37 reports, all under src/habu/native-runtime.f): FILE-SCRATCH-RETIRE omits DECOUPLED-ARENA-SNAP-RESET, which the image seam runs. Add it once every arena it empties is shown per-definition scratch. Test-first frame e2e case. Workspace: carl-ptra, brief ~/.cache/tmp/carl-file/brief-ptra.md.
