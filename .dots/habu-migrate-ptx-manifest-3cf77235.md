---
title: Cut PTX manifest to explicit JSON
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T16:07:53.370522+02:00"
blocks:
  - habu-add-explicit-json-5d7ee868
---

Why: KMAN:MANIFEST$ depends on the singleton and returns a borrowed global buffer. Result: KMAN becomes a renderer that consumes and returns a caller-supplied JSON-WRITE:writer through the final PUT and structure emitters. The kernel-export caller owns writer state, scratch, prefix buffer, and final output. KMAN copies the canonical prefix through JSON-WRITE:COPY before computing manifest_content_hash, then appends the final field. A required copy preserves writer and exact length without hashing or publishing. Delete raw MANIFEST$, package-global writer or output storage, and every singleton call in this consumer. Owner and touch points: lib/ptx/kernel-manifest.f, tools/ptx/kernel-export-lib.f, and their focused tests only. Production red: a second manifest overwrites the first borrowed output. Acceptance: canonical bytes and both hashes stay byte-identical; two manifests interleave; prefix and final one-short destinations remain unchanged with exact requirements; kernel-manifest and kernel-export owning tests pass while unrelated singleton consumers remain unchanged on the feature branch. Forbidden: duplicate serializer, global buffer, raw JSON span, allocation, adapter, compatibility, version, metric, or lint. Smallest owning check: bin/hb --load lib/ptx/kernel-manifest-test.f and bin/hb --load tools/ptx/kernel-export-test.f. Claim: unassigned.
