---
title: Finish the index-to-shape fixture sweep
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T20:28:53.263752+02:00"
---

native-elaborate.f still reads operations by absolute index in other cases (~183-216, ~304-390) — same latent fragility habu-name-ops-by-fb2f42a0 fixed for BUMP-BODY; those bodies are too small for today's memo to touch, but the next op-count-changing transform trips them. Sweep the file with the F-OPC-AT/F-FROM? idiom. Found 2026-08-05.
