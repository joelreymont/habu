---
title: Construct TV-LINEAR output from validated storage
status: active
priority: 1
issue-type: task
created-at: "2026-07-21T22:01:19.331645+02:00"
---

maki/tensor-value.f crosses into the trusted LINEAR writer after checking only the matrix inner dimension. A caller can supply an undersized output descriptor and receive a typed success after writes beyond its declared storage. Define the complete linear contract over input, weight, optional bias, result shape, dtype, layout, address space, alignment, element count, byte capacity, ownership, and alias policy. Prefer an API that validates inputs and an owner-provided storage span, then constructs and returns the only legal result descriptor; do not accept an arbitrary predeclared output descriptor as authority. Cross the trusted kernel boundary only with this validated value; its source-local rationale must name the complete bound, this dot as retirement owner, and the focused production-path test. Add canaries and mutation/property tests for every dimension, dtype/layout/space mismatch, short and overflowed storage, bias extent, overlap, zero sizes, exact fit, and multi-row output; every reject occurs before a write. Preserve valid numerics and executor/device semantics. Files: maki/tensor-value.f, focused tensor-value tests, and direct callers. Verify exact suite, Maki, typed-local diff, package/host/dot lints, and full native gate.

Claim: agent=enumcert_impl workspace=.jj-ws/habu-construct-tv-linear-989c4402.
