---
title: Tick of an undefined name exits 0
status: open
priority: 2
issue-type: task
created-at: "2026-08-20T21:03:58.888972+02:00"
---

From seal-3 (2026-08-20, verified on master): ' UNDEFINED-NAME at top-level interpret exits 0 - tick of an unresolvable token should be E-UNDEFINED by name. This is why the seal's gate cases avoid bare-tick probes on sealed tails (they would pass for the wrong reason). Small engine fix + a gate case; unblocks honest tick coverage in internal-word-gate.
