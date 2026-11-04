---
title: Make reused app images equal fresh ones
status: closed
priority: 1
issue-type: task
created-at: "2026-10-23T17:36:53.497213+03:00"
closed-at: "2026-10-28T17:13:33.350534+02:00"
close-reason: landed 77e3b4f8, 7ee51123
---

Problem: a build that reuses files writes an image whose DATA differs from a fresh-cache build: scratch cells (loop indexes, per-declaration state, derived registry links, builder session state) keep the last-run value, and a hit skips the code that overwrites them (famimg, 2026-10-23: 5 runs with families, 41 in a no-family full-hit control). Acceptance: the types e2e asserts warm full-hit and shift-step app image digests equal fresh-cache digests; each differing cell fixed at its owner (locals, resting values at owner end, canonical at persist, observer reset); no SND-ZERO-LIVE entries for them. Census and fixture: brief ~/.cache/tmp/carl-file/brief-img-census.md.
