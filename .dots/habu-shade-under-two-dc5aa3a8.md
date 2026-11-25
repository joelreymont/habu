---
title: Shade under two casters as rays do
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T19:43:33.726626+01:00"
---

Problem: src/lib/browser/viewport.js keeps only the caster nearest the key in each texel of its shadow map (blocker) and shades with one mean blocker height, so a second caster nearer the key lightens the first one's shadow: at (27.98, 67.67, 0) mm V is 0.43 under box A (0,0,0)..(40,40,40), 0.75 under A and B (38,-232,0)..(118,-152,200), and rays traced through the key's softbox give 0.37; a single box is up to 0.5 off too. Acceptance: V comes from every surface, as the share of softbox rays that clear it; over the shadows of box A, of A and B, of a tall wall beside a low box and of a caster floating above another, at ground points at least 6 mm from a foot, V is within 0.1 of traced rays, and the named point's grey is 211..224; a frame's GPU time grows by no more than a third; a display test in test/browser holds both; STUDIO:KEY-VOLUME agrees with the host. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-shadow.
