---
title: Download GPU buffer
status: active
priority: 1
issue-type: task
created-at: "2026-08-03T09:38:58.808435+02:00"
---

Why: GPT2:LOGITS must copy its device logit row into caller-owned host memory without exposing a raw device pointer. Result: add GPU:DOWNLOAD ( GPU:session GPU:buffer CAD-NUM:byte-off ptr u8 CAD-NUM:byte-len -- GPU:session GPU:buffer result<n,n> ), the exact readback mirror of GPU:UPLOAD. It binds the owning session, validates offset plus length without overflow before advancing the device pointer or calling CUDA, preserves both owners on every refusal, returns the verbatim first CUDA code, and adds no type, callback, registry, plan, compatibility, or model-specific logic. Owner: maki/gpu-buffer.f only. Production red: GPU:DOWNLOAD is undefined. Acceptance: public-entry fake tests cover bind return and throw, zero/end/overflow/out-of-range spans, DTOH return and throw, exact call arguments, unchanged owners, and a real allocate-upload-download-free round trip. Smallest owning checks: focused gpu-buffer test, existing package and typed-local gates.

Claim: agent=codex-gpu-download workspace=.jj-ws/habu-download-gpu-buf-3a26af62
