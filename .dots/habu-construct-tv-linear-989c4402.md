---
title: Construct TV-LINEAR output from validated storage
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:01:19.331645+02:00"
---

maki/tensor-value.f crosses into the trusted LINEAR writer after checking only the matrix inner dimension. A caller can supply an undersized output descriptor and receive a typed success after writes beyond its declared storage. Define the complete linear contract over input, weight, optional bias, result shape, dtype, layout, address space, alignment, element count, byte capacity, ownership, and alias policy. Prefer an API that validates inputs and an owner-provided storage span, then constructs and returns the only legal result descriptor; do not accept an arbitrary predeclared output descriptor as authority. Cross the trusted kernel boundary only with this validated value and retire any false TRUSTED.md assurance. Add canaries and mutation/property tests for every dimension, dtype/layout/space mismatch, short and overflowed storage, bias extent, overlap, zero sizes, exact fit, and multi-row output; every reject occurs before a write. Preserve valid numerics and executor/device semantics. Files: maki/tensor-value.f, focused tensor-value tests, direct callers, TRUSTED.md. Verify exact suite, Maki, typed-local diff, trust/package/host/dot lints, and full native gate.

Claim: RELEASED 2026-07-29 by the stale-claim audit. Agent `enumcert_impl` and workspace `.jj-ws/habu-construct-tv-linear-989c4402` are both gone: the directory does not exist and `jj workspace list` has no record of it. The work has not landed - `maki/tensor-value.f:711` still declares `TV-LINEAR ( tensor tensor tensor tensor -- tensor )` taking a caller-supplied output tensor. The dot stays active and is free to claim.

REOPENED 2026-08-04 (dot-purge): this dot carried `status: active` with no live owner - no `agent=`/workspace claim, or a claim explicitly released. An active dot with no owner is invisible to `dot ready` and holds its id hostage, so the status is now `open` and the dot is free to claim.
