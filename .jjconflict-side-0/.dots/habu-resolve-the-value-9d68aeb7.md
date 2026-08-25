---
title: Resolve the value-list attribute reservation
status: open
priority: 2
issue-type: task
created-at: "2026-08-15T14:07:48.019550+02:00"
---

Live-citation finding (vintage audit; blocks 10440e3e closure): attr.f:24-27 and :221 reserve wire code 9 for a value-list kind that was to land with the ops stage - the ops stage SHIPPED without it, no production consumer exists (frozen.f:245 reads integer attrs only), and wire-code is the deleted codec's vocabulary. Per CG-31: name a real consumer and build it, or rewrite the reservation recording the absence. Files: src/compiler/ir/attr.f. Depends: none; 10440e3e closes behind it.
