---
title: Use canonical checkpoint dtype
status: active
priority: 2
issue-type: task
created-at: "2026-07-29T20:34:14.954473+02:00"
---

Problem: SAFET publishes raw numeric dtype constants while `MAKI:dtype` is the only model dtype. Equal cell values can cross that package boundary without type proof. Result: `SAFET:DTYPE? ( SAFET:census n -- SAFET:census option<MAKI:dtype> )` returns the canonical enum for every supported wire dtype. Wire spelling and element-size decoding remain private to SAFET. Delete the public `SAFET:DT-*` constants; do not retain numeric aliases. Owner: package SAFET in `maki/infer/safetensors.f`; exact tests live in `maki/infer/safetensors-test.f`. Dependencies: current `MAKI:dtype` only. Production red: a checked caller can unwrap `SAFET:DTYPE?` as `n` and compare it with `MAKI:DTYPE>N`; the representative checkpoint must make that caller fail while a typed caller passes. Acceptance: F32, F16, BF16, I32, and U32 fixtures return the exact MAKI enum arms; unknown wire text still throws `SAFET:E-DTYPE`; missing tensor ids return `NONE`; raw numeric and foreign enum consumers fail checker compilation; the real pinned GPT-2 checkpoint reports F32 through `SAFET:LOAD`; `maki/infer/safetensors-test.f`, typed-local, package, trust, and exact-diff gates pass. Forbidden: GPT2LOAD or WSTORE edits, compatibility words, numeric dtype exports, second dtype enum, new trust, device loading, schema, version, fallback, or unrelated cleanup. Smallest owning check: `bin/hb --load maki/infer/safetensors-test.f`. Exact write set: `maki/infer/safetensors.f`, `maki/infer/safetensors-test.f`. Claim: agent=codex-safet-dtype workspace=.jj-ws/habu-use-canonical-checkpoint-92eac785.
