---
title: Stage GPT-2 checkpoint
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:01:46.875486+02:00"
blocks:
  - habu-own-gpt-2-3292019c
  - habu-prove-sealed-inference-1d007ad5
  - habu-own-model-asset-c6f938e4
  - habu-own-device-completion-9aed0a22
---

Why: the pinned safetensors file must be authenticated and copied directly into provisional device storage. Result: package-private GPT2DEV:STAGE takes DEVRT:session, GPT2DEV:stage, MODEL-ASSET:ws, a root ptr u8 plus CAD-NUM:byte-len, and GPT2:config. Its exact result is staged(session,stage,ws) or refused(session,stage,ws,stage-error), where stage-error is path, open, authentication, parse, catalog, copy, synchronization, or close. It preflights root plus GPT2PIN basename against FS-PATH-CAP in the workspace, opens exactly model.safetensors, validates every GPT2 tensor name, shape, orientation, F32 dtype, and extent, and copies through the workspace buffer. Every host-to-device copy completes on the same stream before that buffer is reused. SHA-256 covers the same mapped bytes used for header validation and staging; the source closes before staged returns. Owner: one authenticated checkpoint-to-stage transaction only. Production red: no direct path consumes the pinned file into device storage. Acceptance: all 160 roles stage once; a delayed-copy mutation fails; every named failure returns all provisional owners and publishes no weights; peak host staging is one chunk and its high-water byte count is retained in the eventual weights owner. Forbidden: package-global path or staging buffer, WSTORE, GPT2LOAD, full-host copy, second open, retry state, raw device pointer, pack, version, or compatibility reader. Smallest owning check: bin/hb --load maki/infer/gpt2-device-stage-test.f.
