---
title: Own GPT-2 device weights
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:01:46.791031+02:00"
blocks:
  - habu-own-persistent-inference-ecc98bdf
  - habu-own-gpt-2-14415dcd
---

Why: direct device loading needs one session-bound owner without a GPT2LOAD or WSTORE host model. Result: package GPT2DEV defines a private linear staging owner for one contiguous device region and a public linear weights owner containing the authenticated session generation and exactly GPT2TENSOR:COUNT descriptors. Package-private allocation preflights all config-derived extents before one device allocation; disposal releases provisional stage; only GPT2DEV:LOAD may retype a complete stage into weights. RELEASE-WEIGHTS consumes weights through the matching session. FOOTPRINT returns immutable host and device byte totals from stored extents. Owner: device-weight layout, allocation, footprint, publication type, and release only. Production red: no type owns GPT-2 device weights. Acceptance: first and last descriptor extents are exact; short, overflow, allocation, stale-session, and partial-stage cases publish no weights and release once; FOOTPRINT equals the allocation plan; two sessions coexist. Forbidden: public stage, public publish word, WSTORE, GPT2LOAD, host mirror, per-tensor allocation, raw device pointer, pack, version, or compatibility path. Smallest owning check: bin/hb --load maki/infer/gpt2-device-weights-test.f.
