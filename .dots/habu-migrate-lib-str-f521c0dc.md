---
title: "Migrate library string callers to STR:"
status: open
priority: 3
issue-type: task
created-at: "2026-07-15T15:05:05.423165+02:00"
---

Full context: MODEL-CAD-V2-PLAN.md B5.5a legacy-STR census, library lane. Migrate every raw STR call in: examples/string-regex.f (INDEX-OF), lib/json-read.f (INDEX-OF), lib/float.f (INDEX-OF), lib/process-env.f (INDEX-OF, SPLIT-NEXT), lib/object.f (SPLIT-NEXT), lib/ptx/ad.f (SPLIT-NEXT), lib/content-key.f (BUF-APPEND-LEN -> STR:BUF-APPEND dropping the >LEN pre-conversion). option<CAD-NUM:index> returns; SPLIT-NEXT overflow guard; behavior byte-identical. Acceptance: fresh rg census empty in these files; each focused test green. Files: the 7 listed + focused tests. Verify: focused tests, library gate slice. Ownership: the 7 library files.
