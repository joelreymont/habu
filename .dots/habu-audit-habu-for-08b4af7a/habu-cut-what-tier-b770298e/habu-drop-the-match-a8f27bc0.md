---
title: Drop the match-side wide pad refusal no lowering needs
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T18:21:34.125069+01:00"
---

Problem: src/core/type-family.f refuses a match of a wide-family value whose instantiated pads are fewer than the declared (the of-side refusal, ~4945), saying a declared-width unpack would skip a pad, but T0's ARM-OF (src/habu/t0.f) drops the instantiated pads MWIN-CELLS! filed and never reads declared pads; TFC-XPAD-NARROW-REJECT's header says the lowering pushes the declared pads, which is false for a match. Acceptance: the match certifies and runs with the right payload, or the refusal is shown needed and its comment made true. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-fu5.
