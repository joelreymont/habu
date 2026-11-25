---
title: "Own a font's rendered image"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T20:06:12.759396+01:00"
---

Problem: FONT:RENDER answers an image its caller must FONT:FREE on every path (src/lib/font.f FREE), a release word its callers must remember; Maki calls FONT:FREE 24 times in src/drawing.f, each under a hand-written finally, and missed one. Acceptance: the image's lifetime is a C2 owner scope or a DEFLINEAR value the checker holds, a program that drops an image without releasing it is rejected, and FONT:FREE is gone; tim (Maki) moves its callers. Depends: none. Claim: unassigned. Requested by tim.
