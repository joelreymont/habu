---
title: Refresh the engine census ratchet for the native chain
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-31T20:50:45.722543+02:00\""
---

The engine build slice's census self-check is red on the proofs branch: STATUS.md pins 4232 definitions and the tree now measures 4265, because the native-chain campaign added compiler files without refreshing the ratchet. Found by the gate-scheduling lane running test/run.f. This blocks any future master merge (master is always green). Refresh the pinned number the way previous ratchet refreshes did - deliberately, with the delta accounted for in the commit message - and re-run the engine build slice green. Nothing else in the slice was reported red.

Claim: agent=ratchetlane workspace=.jj-ws/habu-refresh-the-engine-f2d8e5d8
