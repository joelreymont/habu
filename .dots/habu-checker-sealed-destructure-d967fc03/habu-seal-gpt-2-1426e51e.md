---
title: Make GPT-2 tensor identity owner-only
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:36:41.189664+02:00"
---

Owner: package GPT2 after copied config-key deletion. In one atomic hard cut, mark the one-cell nominal layer-id CONSTRUCT owner, replace internal MAKE calls with construct layer-id, publish no LAYER--ID:MAKE, and delete layer-proof, its TRUSTED mint, proof plumbing, and tests. Keep public UNMAKE, tensor-id enum constructors, role/slot queries, tensor catalog, shape/orientation authority, and consuming-config bounds. No intermediate proofless public constructor may commit. Add no replacement token, digest, trusted cast, hidden MAKE, runtime guard, or compatibility arity. Production red: public LAYER--ID:MAKE permits foreign identity minting and layer-proof exists only to compensate. Acceptance: the representative diff already removes both MAKE and the proof ceremony; LAYER--ID:MAKE and every proof symbol do not resolve; foreign and reopened packages cannot mint a layer identity; layer zero/last and wrong-bound fixtures, catalog calls, hostile transaction rollback, AOT/fixpoint, and exact-diff gates pass. Smallest owning check: the GPT2 tensor catalog test plus one checked foreign layer-id construction negative on the same diff. Claim: unassigned.
