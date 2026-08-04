---
title: Own persistent inference session
status: closed
priority: 2
issue-type: task
created-at: "2026-07-29T20:40:51.505656+02:00"
closed-at: "2026-08-04T19:31:35.810142+02:00"
close-reason: "Superseded by GPT2:model: landed GPU session, one device buffer, config, modules/functions, tokenizer, inference state, and exact reverse close already compose one linear owner; a DEVRT session would duplicate that lifetime."
blocks:
  - habu-own-device-runtime-5e7d559e
  - habu-load-gpt-2-6b12b3fe
  - habu-own-gpt-2-b40bdd2c
---

Problem: the proven GPT-2 core, module inventory, and storage plan need one public linear session. Result: DEVRT:OPEN-GPT2 consumes the private core, modules, and storage products and publishes the gpt2 arm of DEVRT:session only after their generations and complete inventories match. CLOSE-GPT2 consumes that arm, synchronizes, and releases storage, modules, stream, and scope in reverse order. Device completion state is added by its separate leaf. Owner: GPT-2 runtime composition and session publication only. Production red: the three private owners cannot be launched as one persistent session. Acceptance: two sessions coexist; repeated launches reuse identical module, function, and device addresses; every composition and close failure releases or returns each owner exactly once; FOOTPRINT equals the sum of the storage and module owners. Forbidden: CUDA acquisition, module compilation, extent planning, allocation, second CUDA scope, per-token work, fixed cap, global handle, model registry, ABI version, compatibility target, or wrapper path. Smallest owning check: bin/hb --load maki/infer/gpt2-runtime-test.f on DGX Spark. Claim: unassigned.
