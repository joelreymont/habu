---
title: Compile nonreturning native loops with valid memory order
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-13T22:15:32.753596+03:00\""
closed-at: "2026-09-14T13:12:35.007569+03:00"
close-reason: Fresh all-AOT native product accepts pure nonreturning loops and the returning arm of a mixed CFG. The order validator permits a final token only before a region with no reachable return or memory-order operation. Real hand-built tests reject backedges to the defining operation, discarded order before return, and a later independent memory chain. Full native-regalloc and native-order-exit tests pass; complete combined gate remains tracked by the campaign.
---

Independent D04 review reproduced a pre-existing native validator refusal: 1 set-tier : FOREVER ( -- ) begin again ; exits67 / E-A64RAV-ORDER (-8522) on untouched seed5b969df8, parent59eebc18 and current source compiler. No execution of the nonreturning word is needed. Fix order validation/production for reachable infinite cycles at the responsible native layer, preserving rejection of malformed order. Logs /tmp/cedar-D04-review-*.log. This is independent of the live counted-loop obligations repair and its required formal-model update.
