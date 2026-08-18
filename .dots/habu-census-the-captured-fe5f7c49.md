---
title: "Census the captured DATA window: live bytes vs baked emptiness"
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T14:04:04.550453+02:00"
---

The product bin/hb is 3.65MB; the largest single component is the 1,531,045-byte captured chain DATA window, never audited for composition. Question: how much is LIVE data (tables, interned strings, initialized structures) vs cap-sized allotments persisted at their full size while mostly empty (arenas allotted at cap, buffers with low watermarks)? Method: walk the window with the record readers + the residue tools, classify by owning allotment (snap-heap-owner + the create-owner map), report bytes live / bytes zero / bytes cap-slack per owner. If slack dominates, the fix candidates are: capture live extents with per-buffer watermarks (the arena pattern), or zero-run compression in the artifact + sparse seed. This decides whether the post-cut product is ~2MB or approaches the engine+compiler-code floor (~460KB + pools). Feeds the user's <200k bar: engine hb-host IS 165KB; the product's path under 1MB runs through this census + the post-cut recapture (1.2MB code -> ~295KB, measured 4.1x).

USER RULING 2026-08-18 (direct, supersedes the census-decides
framing): HB CREATES ITS BUFFERS AT STARTUP; THE BINARY IS
TIGHT CODE ONLY. The design is settled: the artifact and the
seed ship CODE + genuinely initialized constant data and
NOTHING else - every arena, scratch buffer, and cap-sized
allotment is CREATED AT BOOT, not persisted. The census's job
is now the implementation map, not a decision: per owner,
classify initialized-constant (ships) vs created-at-boot
(deleted from the payload), and the fix follows immediately -
capture initialized extents only, the seed allots the rest.
Expected outcome: the DATA section collapses from 1.53MB to
the constant tables' true size; the product approaches
engine + dense code + constants + signatures.

