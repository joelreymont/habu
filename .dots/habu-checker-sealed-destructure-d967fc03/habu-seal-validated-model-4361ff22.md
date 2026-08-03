---
title: Make model config owner-only
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:36:41.100566+02:00"
blocks:
  - habu-publish-owner-product-32b3f03c
  - habu-delete-model-config-1c71a13e
  - habu-delete-unused-config-a6f7d6dc
---

Owner: package GPT2 after schema-version and unused-config-identity deletion. In one atomic hard cut, move the config structure declaration into the owner package's private section so construction resolves only inside the owner (no new checker machinery — the CONSTRUCT-owner substrate was cut from the milestone), publish no GPT2-CONFIG:MAKE, and delete the proof field, model-proof type, TRUSTED mint, proof checks, and tests. Keep public UNMAKE and current semantic projections. No intermediate proofless public constructor may commit. Add no replacement token, digest, trusted cast, hidden MAKE, forwarding word, runtime guard, or compatibility arity. Production red: the current public MAKE permits foreign reconstruction and the private proof exists only to compensate. Acceptance: the representative diff already removes both MAKE and the proof ceremony; GPT2-CONFIG:MAKE and every proof symbol do not resolve; GPT2-CONFIG:UNMAKE remains readable but foreign and reopened packages cannot reconstruct; valid HF:OPEN-GPT2 intake, projections, hostile transaction rollback, AOT/fixpoint, and exact-diff gates pass. With construction sealed, delete any remaining forged-config test fixtures (public UNMAKE/MAKE reconstruction) in the same cut — the seal makes them inexpressible. GPT2:COUNT's census bound stays: after the catalog cut it is the sole census authority, not a duplicate. Smallest owning check: the production HF and GPT2 config tests plus one checked foreign reconstruction negative on the same diff. Claim: unassigned.
