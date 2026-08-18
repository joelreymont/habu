---
title: "Census the captured DATA window: live bytes vs baked emptiness"
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T14:04:04.550453+02:00"
---

The product bin/hb is 3.65MB; the largest single component is the 1,531,045-byte captured chain DATA window, never audited for composition. Question: how much is LIVE data (tables, interned strings, initialized structures) vs cap-sized allotments persisted at their full size while mostly empty (arenas allotted at cap, buffers with low watermarks)? Method: walk the window with the record readers + the residue tools, classify by owning allotment (snap-heap-owner + the create-owner map), report bytes live / bytes zero / bytes cap-slack per owner. If slack dominates, the fix candidates are: capture live extents with per-buffer watermarks (the arena pattern), or zero-run compression in the artifact + sparse seed. This decides whether the post-cut product is ~2MB or approaches the engine+compiler-code floor (~460KB + pools). Feeds the user's <200k bar: engine hb-host IS 165KB; the product's path under 1MB runs through this census + the post-cut recapture (1.2MB code -> ~295KB, measured 4.1x).
