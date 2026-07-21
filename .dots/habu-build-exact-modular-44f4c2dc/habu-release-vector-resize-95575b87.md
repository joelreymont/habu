---
title: Release vector resize storage
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-15T23:49:39.979874+02:00\\\"\""
closed-at: "2026-07-21T07:41:15.398686+02:00"
close-reason: "Landed in stack cb1e4cae: vector resize now releases the old mapping after successful install (single shared install point covers all grow/ensure paths; release is the LAST step so a failed grow keeps the old storage intact), plus packaged VEC:DISPOSE with capacity-as-ownership-token (clear-before-release makes repeated dispose a proved no-op per the cuda-scope consume-on-release precedent) and named E-VEC-STATE use-after-dispose rejects at all three storage-touching surfaces. Red-first both ways: address-reuse probes prove the base leaked and the fix reuses exactly; stripping the release logic reds 11 shipped assertions; over-max resize throws E-MEM-MAP with the old vector byte-intact. Loose end dotted: VEC-INIT on a live header still leaks (re-init vs dispose is its own concern)"
blocks:
  - habu-expose-checked-mmap-06c1d522
---

Full context: lib/vector.f VEC-RESIZE copies into a new mapping and abandons the old allocation; vectors have no dispose operation. After MEM:RELEASE-BYTES lands, release the prior capacity after a successful copy/install, add packaged VEC:DISPOSE, clear header ownership exactly once, and preserve failure atomicity. Acceptance: repeated grow/resize/dispose has bounded resident mappings, double dispose and use-after-dispose fail with named errors, allocation/copy failure leaves the old vector valid, raw and packaged vector tests green. Files: lib/vector.f, lib/vector-test.f, errors/inventory only if a named state error is missing.

Claim: agent=vecrel workspace=.jj-ws/fable-vecrel machine=spark (owns lib/vector.f resize release + VEC:DISPOSE + tests, consuming the landed MEM:RELEASE-BYTES 541b691f)
