---
title: Reconcile engine trust ratchet
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-16T13:19:44.487918+02:00\""
---

Full context: on PTY branch commit 9233bac, bin/hb --load tools/trusted-inventory-test.f fails F12 because TRUSTED.md file-level row test/engine-suite.f ... habu-seal-set-check-b3676b33 declares 6 but current exact live sites are 7 at test/engine-suite.f:306,669,707,754,1024,1863,1999. Prove each site belongs to the cited capability, update the ratchet count only if exact, add/adjust focused regression so stale count fails, and run trusted inventory plus host/filemap. Dependency: PTY stack through 9233bac. Acceptance: zero GREW/STALE, focused inventory and lints green.
