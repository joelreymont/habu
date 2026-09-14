---
title: Capture an unassigned defer without borrowing the producer prefix
status: active
priority: 2
issue-type: task
created-at: "2026-09-14T03:40:59.034378+03:00"
---

The current cold WID producer declares defer AWB-VEC and AWB-CALL, then capture refuses its registered nonzero default target. Exact reduced actual cold run prints defer-unset=24189424; address row 296 at DATA+9245696 carries that same target outside the captured code range 25065012..25075020. Logs /tmp/cedar-wid-trap-capture.{out,err}; source /tmp/cedar-wid-trap-capture.f. Full current-writer WID suite exits rc1 (F112), not timeout: the final data-span helper fails HABU_AOT_TRAP before BIG/EXT/PREWIN controls run. Both genuine WID collision cases and earlier protection/span controls passed. Preserve the execution claim for an unassigned defer and strict target self-containment; no fixture-local trap or cleared-cell workaround. Root owns responsible-layer investigation and review.
