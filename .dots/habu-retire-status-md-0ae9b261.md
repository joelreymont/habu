---
title: Retire STATUS.md and the census ratchet
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T14:33:54.005951+02:00"
---

Master's governance cleanup deleted STATUS.md entirely and its AGENTS.md carries no reference to it; proofs still has both, plus the census ratchet in test/gate-engine-lib.f:534-585 that pins a committed STATUS.md count against the measured self-check census — a committed-pin ledger of exactly the class the cleanup removed, and it is RED on this host (recorded 4270, measured 4278+) purely as accumulated bookkeeping debt (dot habu-re-measure-the-fbb647d3 documents the owed rows). Mirror the deletion onto proofs: delete STATUS.md, remove the census ratchet check from the engine build slice (and any other STATUS.md reader), update AGENTS.md line 7 to master's wording (drop the STATUS.md sentence), close habu-re-measure-the-fbb647d3 as moot, and sweep .dots for other dots whose only substance is STATUS.md rows. Keep the self-check census MEASUREMENT itself if the build slice uses the number for anything real (a die on 'uncheckable/rejected > 0' is behavior, not a ledger — keep that half if present; only the recorded-vs-measured ratchet goes).
