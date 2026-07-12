---
title: Reject stale tensor handles after reset
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T07:50:58.369561+02:00"
blocks:
  - habu-v2-r3-type-9f89d1e9
---

Problem: TENSOR:TV-RESET sets TV-U to zero, so a stale tensor handle remains an in-range raw index and can silently alias a newly allocated descriptor; TV-IX checks only current count. This violates provenance/generation validation required by MODEL-CAD-V2-PLAN.md R3. Fix: encode a monotonic non-wrapping store generation plus slot into each one-cell tensor handle (named bit/limit constants), increment generation on reset, and make every accessor reject generation mismatch before indexing; fail closed before generation overflow. Do not expose raw pack/unpack publicly. Acceptance: stale handle after reset rejects E-TV-HANDLE even when the same slot is reused; current handle remains valid; forged generation/index, capacity, and generation-overflow fixtures reject; plan store cannot resurrect stale tensor inputs; no raw n public handle. Files: maki/tensor-value.f/test and dependent handle-render tests; TRUSTED.md only if a private representation axiom is unavoidable. Verify: tensor-value/plan focused tests, typed-local diff lint, maki/test.f, trust-lint, host-lint, filemap-lint.
