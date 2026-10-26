---
title: Retire pointer-term arenas at file boundaries
status: closed
priority: 1
issue-type: task
created-at: "2026-10-26T13:24:06.648344+02:00"
closed-at: "2026-10-26T15:51:10.504605+02:00"
close-reason: "obsolete: dropped unlanded (rqmplkyw abandoned). Subsumed by habu-take-checker-working-6703bc4f: the wholesale region retire replaces a per-file arena reset, and this fix leaked every grown mapping (ARENA-SNAP-BOOT never unmaps)."
---

First earlier-write site PTRA-BOOT+8 (37 reports, all under src/habu/native-runtime.f): FILE-SCRATCH-RETIRE omits DECOUPLED-ARENA-SNAP-RESET, which the image seam runs. Add it once every arena it empties is shown per-definition scratch. Test-first frame e2e case. Workspace: carl-ptra, brief ~/.cache/tmp/carl-file/brief-ptra.md.
