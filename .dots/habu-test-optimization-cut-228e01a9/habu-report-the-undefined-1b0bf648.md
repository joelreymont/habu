---
title: Report the undefined word under tier 1
status: closed
priority: 3
issue-type: task
created-at: "2026-10-04T04:41:56.233903+03:00"
closed-at: "2026-10-22T12:36:34.791034+03:00"
close-reason: "Triage 2026-10-22, DONE: At tier 1 (`1 set-tier`, both loops) the probe reports \"undefined word 'NOPE'\", not 'if'."
---

Problem (lane 573, on 7c03e870): at tier 1, `: X ( -- ) 0 if NOPE then ;` is reported as `undefined word 'if'` ($HOME/.cache/tmp/kestrel-jerry-ctlflow/pr/cap2.err), naming a defined control word instead of the undefined NOPE. Fix: find the layer that substitutes the structure's opener for the failing token and report the token that failed. Acceptance: the probe names NOPE with its location, rc 70, at tier 1 and tier 0; seen naming 'if' first; baked if the fix is: rebuild, g1 == g2 with .names, two-generation build. After: 7a137417.
