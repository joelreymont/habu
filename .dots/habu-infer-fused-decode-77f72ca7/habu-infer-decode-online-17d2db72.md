---
title: "Infer decode: online softmax oracle"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.345131+02:00"
blocks:
  - habu-infer-decode-supported-29bebe81
---

Why this exists:
contiguous and paged kernels need one numerically explicit online-softmax recurrence and high-precision oracle.

Required result:
package DECODE-REF implements the running maximum, rescaled sum, and value accumulator for one causal F32 query with F64 comparison accumulation. It consumes DECODEGEOM geometry and caller-owned Q/K/V buffers; it owns no model, cache, or device code.

Done when:
empty, one-token, masked, extreme-logit, and long-context fixtures agree with the high-precision reference under derived tolerance; no device code in this leaf.

Expected touch points: new maki/infer/decode-reference.f and focused test.
Smallest check: bin/hb --load the focused reference test.
Prerequisites: supported geometry contract.
Owned result: host online-softmax oracle only.
Claim: unassigned.
