---
title: Shade under two casters as rays do
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T19:43:33.726626+01:00"
---

Problem: src/lib/browser/viewport.js keeps only the caster nearest the key in each texel of its shadow map (blocker, :349-360) and shades with one mean blocker height (:479-493), so a second caster nearer the key lightens the first one's shadow: at (27.98, 67.67, 0) mm V is 0.43 under box A (0,0,0)..(40,40,40), 0.75 under A and B (38,-232,0)..(118,-152,200), and rays traced through the key's softbox give 0.37. Acceptance: at that point and over the two boxes' shadows the host's V is within 0.1 of traced rays, a single caster's shadow does not change, and a frame's GPU time grows by no more than a third; a display test in test/browser holds the two boxes. Depends: none. Claim: unassigned.
