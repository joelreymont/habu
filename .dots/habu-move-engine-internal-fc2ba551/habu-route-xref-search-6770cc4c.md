---
title: Route xref-search-wl through CHECKER-RESOLVE
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.851346+01:00"
---

Problem: xref-search-wl has a global row and owner rows in several packages (src/habu/prims.f). Acceptance: its one private row is CHECKER-RESOLVE's; its other callers (src/habu/xref.f, src/compiler/native/dict.f, src/habu/interpreter.f) call the public CHECKER-RESOLVE:WL-PROBE; no global row. Depends: none. Claim: unassigned.
