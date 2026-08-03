---
title: Load GPT-2 device weights
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:40:51.426546+02:00"
blocks:
  - habu-own-gpt-2-3292019c
  - habu-stage-gpt-2-e90c6fcb
  - habu-own-model-asset-c6f938e4
---

Problem: the completed private GPT-2 stage has no all-or-nothing publication transaction. Result: package GPT2DEV owns LOAD ( DEVRT:session MODEL-ASSET:ws ptr u8 CAD-NUM:byte-len GPT2:config -- GPT2DEV:load-result ). Its exact result is loaded(session,weights,ws,high-water) or refused(session,ws,load-error), where load-error is one closed enum: allocation, path, open, authentication, parse, catalog, copy, synchronization, completeness, publication, or close. LOAD allocates one private stage, calls STAGE exactly once, synchronizes the same session, proves every GPT2 tensor descriptor was filled once, and total-retypes the stage into GPT2DEV:weights only on success. Refusal releases the provisional region and returns the workspace and session; STAGE already closed its source. RELEASE-WEIGHTS and immutable FOOTPRINT remain owned by the weight-lifetime leaf. Owner: direct GPT-2 load composition and sole publication point only. Production red: no product call publishes authenticated GPT-2 device weights. Acceptance: the real root publishes one weights owner with selected words matching the pinned source; every named failure publishes nothing, releases once, and returns session and workspace; high-water equals the measured peak; zero SAFET, GPT2LOAD, WSTORE, stage, or host-weight owners remain. Forbidden: checkpoint parsing, second staging buffer, WSTORE, GPT2LOAD input, second CUDA scope, full-host copy, raw device pointer, pack, lazy load, retry state, version, or compatibility reader. Smallest owning check: bin/hb --load maki/infer/gpt2-device-load-test.f on DGX Spark. Claim: unassigned.
