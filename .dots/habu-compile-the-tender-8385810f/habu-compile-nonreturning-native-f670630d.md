---
title: Compile nonreturning native loops with valid memory order
status: open
priority: 1
issue-type: task
created-at: "2026-09-13T22:15:32.753596+03:00"
---

Independent D04 review reproduced a pre-existing native validator refusal: 1 set-tier : FOREVER ( -- ) begin again ; exits67 / E-A64RAV-ORDER (-8522) on untouched seed5b969df8, parent59eebc18 and current source compiler. No execution of the nonreturning word is needed. Fix order validation/production for reachable infinite cycles at the responsible native layer, preserving rejection of malformed order. Logs /tmp/cedar-D04-review-*.log. This is independent of the live counted-loop obligations repair and its required formal-model update.
