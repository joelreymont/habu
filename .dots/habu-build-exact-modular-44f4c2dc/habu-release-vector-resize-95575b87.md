---
title: Release vector resize storage
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-15T23:49:39.979874+02:00\""
blocks:
  - habu-expose-checked-mmap-06c1d522
---

Full context: lib/vector.f VEC-RESIZE copies into a new mapping and abandons the old allocation; vectors have no dispose operation. After MEM:RELEASE-BYTES lands, release the prior capacity after a successful copy/install, add packaged VEC:DISPOSE, clear header ownership exactly once, and preserve failure atomicity. Acceptance: repeated grow/resize/dispose has bounded resident mappings, double dispose and use-after-dispose fail with named errors, allocation/copy failure leaves the old vector valid, raw and packaged vector tests green. Files: lib/vector.f, lib/vector-test.f, errors/inventory only if a named state error is missing.

Claim: agent=vecrel workspace=.jj-ws/fable-vecrel machine=spark (owns lib/vector.f resize release + VEC:DISPOSE + tests, consuming the landed MEM:RELEASE-BYTES 541b691f)
