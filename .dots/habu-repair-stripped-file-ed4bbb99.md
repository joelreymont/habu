---
title: Repair stripped FILE child loading
status: active
priority: 1
issue-type: task
created-at: "2026-10-15T05:27:51.733063+03:00"
---

With matching native 6ef44489 and reviewed active-scope fix 990065ac, an empty or simple MAIN stripped app builds and runs, but an app requiring a package leaf.f fails rc67 with E-LAYOUT-BOUNDS (7122). Existing native-file-stripped-path E2E reproduces after the scope fix; no executable is produced for the leaf app. Localize the first guard at the responsible compiler, checker, runtime or build boundary and repair it without weakening layout checks or caller workarounds. Reuse the existing real stripped app, GETENV and FILE path E2Es; execute fresh artifacts, then required shared-core gates if baked behavior changes. Carl owns integration and independent review.
