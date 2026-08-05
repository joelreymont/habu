---
title: Probe the package-open scans
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T20:56:29.637405+02:00"
---

C-PACKAGE-ENSURE and the prot/seal guards still linear-scan the namespace chain on the rare package-open path — the same indexed probe CG-26 landed applies. Low frequency, so measure before assuming it matters. Found by the lookup lane 2026-08-06.
