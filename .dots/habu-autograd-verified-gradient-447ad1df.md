---
title: "Autograd: verified-gradient superiority matrix (every VJP type-checked + gradchecked, committed)"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T23:00:02.527284+02:00"
---

The concrete 'better than PyTorch' proof: a committed harness that, for EVERY VJP-table entry, (1) certifies the derived backward kernel through the checker (type-checked - PyTorch backwards are not), and (2) numerically gradchecks it (finite diff within tol). Output a reproducible matrix (op x {certifies, gradchecks-pass}). PyTorch has neither static check; this is the differentiator. Files: a checked Habu tool maki/ad-verify.f + test over the full VJP table; reuse maki/autograd-test.f + ad-test.f patterns; device leg reuses habu-ptx-ad-device. VERIFY: matrix all-green from the committed tree; a deliberately-wrong VJP fails the gradcheck leg (negative regression). Dep: EPIC; composes habu-ad-vjp-primitive, habu-ptx-ad-device.
