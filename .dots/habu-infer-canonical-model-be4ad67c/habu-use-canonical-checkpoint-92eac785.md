---
title: Use canonical checkpoint datatype
status: closed
priority: 2
issue-type: task
created-at: "2026-07-29T20:34:14.954473+02:00"
closed-at: "2026-08-03T00:31:37.203151+02:00"
close-reason: Canonical SAFET datatype comparison landed as 6e82e3a4; full Maki and native stdlib/PTX passed on the exact tree.
blocks:
  - habu-delete-unused-model-40bb32df
---

Problem: SAFET publishes raw numeric datatype constants while `MAKI:datatype` is the only model datatype. Equal cell values can cross that package boundary without type proof. Result: `SAFET:DATATYPE= ( SAFET:file n MAKI:datatype -- SAFET:file bool )` compares one tensor's private wire datatype with the caller's canonical expected datatype. A missing tensor id or a different datatype returns false. Wire spelling, wire tags, and element-size decoding remain private to SAFET. Delete public `SAFET:DT-*`, `SAFET:DTYPE?`, and `SAFET:DTYPE=` without aliases. Owner: package SAFET in `maki/infer/safetensors.f`; exact product tests live in `maki/infer/safetensors-test.f`. Dependency: the sole raw numeric consumer was deleted with the unused host model path. Production red: the runtime dictionary still exposes `SAFET:DTYPE?` and `SAFET:DT-*`, while no typed `SAFET:DATATYPE=` boundary exists. Acceptance: F32, F16, BF16, I32, and U32 fixtures match only their exact MAKI enum arm; unknown wire text still throws `SAFET:E-DTYPE`; missing tensor ids return false; direct runtime dictionary assertions with a positive control prove `SAFET:DATATYPE=` exists while retired public and private wire names are inaccessible; untyped or foreign expected values fail checker compilation; standalone module load owns every dependency; the real pinned GPT-2 checkpoint matches `MAKI-DATATYPE:DF32` through `SAFET:LOAD`; focused SAFET, typed-local, package, full Maki, native stdlib, and PTX gates pass. Forbidden: a generic option/checker expansion, GPT2LOAD or WSTORE edits, compatibility words, numeric datatype exports, second datatype enum, new trust, device loading, schema, version, fallback, or unrelated cleanup. Smallest owning check: `bin/hb --load maki/infer/safetensors-test.f`. Exact product files: `maki/infer/safetensors.f`, `maki/infer/safetensors-test.f`. Claim: agent=codex-safet-dtype workspace=.jj-ws/habu-use-canonical-checkpoint-92eac785.
