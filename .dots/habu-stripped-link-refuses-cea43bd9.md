---
title: Stripped link refuses a SHA256 data address after 75c797be
status: open
priority: 1
issue-type: task
created-at: "2026-09-22T16:48:07.894939+03:00"
---

Regression measured by aspen on the Tender standalone stripped link (notes and logs: ~/.cache/tender/habu-gaps/stripped-mapped-cell/notes.md, standalone-b.log, standalone-after.log). At 75c797be with engine 6dc6cd7b `build --stripped` links clean in 56.9 s (bin/tender 1,507,520 B, a relink byte-identical). At 6aa16d76 with engine ba037929 the same link refuses after 45 s: `aot: address refers to data outside the restored span caller=SHA256 region-off=3879412 value=13964704360 target=SHA-U span=[13969873664,13973480784]` (src/core/sha256.f:58, the byte count SHA256-UPDATE works through). Acceptance: reduce it to a Habu fixture (a stripped hb-build of a program that requires src/core/sha256.f and hashes through SHA256), bisect 75c797be..6aa16d76 with rebuilt engines, fix the responsible layer with the fixture in the gate, and confirm the Tender standalone link is clean again. Ownership: hazel.
