---
title: Captured DATA cell holding a window DATA address is stale after seed
status: open
priority: 2
issue-type: task
created-at: "2026-08-16T18:31:08.774844+02:00"
---

Adjacent finding from the BMID fix (bake-chain-14, 2026-08-16): the seed rebases CODE chains (dsites/csites) but a captured DATA cell whose VALUE is a window DATA address would be stale under the seed's delta - nothing refuses it. The chain appears not to do it (everything green), but 'appears' is a survey, not an audit. Probe first: can the capture detect the class structurally (a recorded DATA cell whose value lies in [d0,d1) is either a declared address cell - already handled via xtoffs - or this hazard)? If the population is provably empty today, land the refusal fail-closed with a forge fixture; if not empty, the members need the dsite treatment. Same family as the four relocation classes; the audit belongs in ACAP.

RE-SCOPE AFTER BUFFERS-AT-STARTUP (2026-08-18): the class (captured DATA cell holding a window DATA address) shrinks with the window itself - re-measure the population on the constants-only capture before building the audit.
