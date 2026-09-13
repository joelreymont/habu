---
title: Remove test requirements to retain unused inliner helpers
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T14:51:13.076942+03:00"
---

Cedar review of d88e3efd on 5226a994: tools/c-call-emitter-test.f:28-54 demands retired C-CALL helpers remain, including exact one-occurrence assertions proving they have no callers. Deleting dead code therefore fails c-call-emitter-shape although live C-CALL is direct BL. Delete obsolete helper-presence/count assertions and the unused helper block; retain meaningful direct-call behavior coverage. Rough reduction 80–100 lines. Coordinate with rowan-tier, which already deletes the inliner. Unassigned.
