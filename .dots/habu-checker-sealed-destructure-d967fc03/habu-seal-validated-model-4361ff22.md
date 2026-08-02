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

Owner: package MDLCFG after schema-version and unused-config-identity deletion. In one atomic hard cut, move the mcfg structure declaration into the owner package's private section so construction resolves only inside the owner (no new checker machinery — the CONSTRUCT-owner substrate was cut from the milestone), publish no MCFG:MAKE, and delete the proof field, model-proof type, TRUSTED mint, proof checks, and tests. Keep public UNMAKE and current semantic projections. No intermediate proofless public constructor may commit. Add no replacement token, digest, trusted cast, hidden MAKE, forwarding word, runtime guard, or compatibility arity. Production red: the current public MAKE permits foreign reconstruction and the private proof exists only to compensate. Acceptance: the representative diff already removes both MAKE and the proof ceremony; MCFG:MAKE and every proof symbol do not resolve; MCFG:UNMAKE remains readable but foreign and reopened packages cannot reconstruct; valid HFCFG builds, projections, hostile transaction rollback, AOT/fixpoint, and exact-diff gates pass. With construction sealed, delete GPT2TENSOR:COUNT's duplicate census guard and any forged-config test fixtures in the same cut — the seal is the invariant that makes them redundant, so they must not survive it or land before it. Smallest owning check: the production HFCFG model-config test plus one checked foreign reconstruction negative on the same diff. Claim: unassigned.
