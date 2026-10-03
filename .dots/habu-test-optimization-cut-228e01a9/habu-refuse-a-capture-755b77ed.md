---
title: Refuse a capture with a definition open
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T00:07:43.856293+02:00"
---

Problem: VERIFY-QUIESCENT (src/habu/snap-lib.f:561) refuses a capture only when CF-DEPTH or JIT-SNAP:SP-CELL is non-zero. A definition open in interpretation state (`: X [ ... capture ... ] ;`) or with a quotation pending has no control frame, so PEND-CELL (layout.f:480), QPATCH-CELL (layout.f:1179) and the JIT-QUOT depth cell are not consulted and the image captures half-built compiler state. Acceptance: first write a case that captures with each of these open and show it is not refused on the parent engine; then VERIFY-QUIESCENT refuses each by name ("snap: active compiler state at capture", rc 74), and a quiescent capture still succeeds. Files: src/habu/snap-lib.f, src/habu/layout.f (read only), test/snapshot-writer-tail.f or a new focused case. Verify: snap-lib.f loads from source in the app-image build; rebuild only if a baked file changes; run the focused snapshot tests.
