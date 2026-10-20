---
title: Preserve build output pairs
status: active
priority: 1
issue-type: task
created-at: "2026-10-16T12:09:18.740221+03:00"
---

Independent review of the unified engine builder confirms installation publishes a new sidecar before copying/replacing the executable, so executable-copy or rename failure can leave old output with new map; artifact restore has the same ordering. Stage both files first and preserve the prior map if executable replacement fails, through the existing filesystem staging boundary. Inspect existing CLI/cache failure cases and extend the narrow real path before changing code; no general publishing framework. Source/refusal and artifact-restore behavior plus independent Astra review required.
