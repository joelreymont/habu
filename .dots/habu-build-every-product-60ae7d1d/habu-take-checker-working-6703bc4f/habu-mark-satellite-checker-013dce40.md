---
title: Mark satellite checker scratch spans
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T15:50:44.513538+02:00"
blocks:
  - habu-hold-per-def-8d5c52f1
---

Lane 4: lower-cert-base.f BUF/KEPT/BODY-*, type-family/render/sumtype/layout-valid REG-*-SCRATCH-XT hooks that only zero scratch become spans (cell-effects.f row for the markers). The top-level typed row (top-row.f) survives require and : by design (ttop-design.md), so it is NOT scratch; lane waits for typed top-level storage. Measure a self-build before/after. Claim: unassigned.

carl 2026-10-30 (lane SCRATCH): about 100 checker temporaries remain outside spans after SCRATCH (list: .jj-ws.noindex/carl-fscr/.out/l11/a2/temps.txt; copy it here before carl-fscr retires). They need one rest mechanism, not one span line each.
