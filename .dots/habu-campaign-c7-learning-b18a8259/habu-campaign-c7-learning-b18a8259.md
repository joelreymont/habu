---
title: "Campaign C7: learning material and hygiene"
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T13:54:06.643687+03:00"
---

Problem: docs/forth.md is the one document an agent must read and tools/public-signatures.f already extracts public signatures, but the reference is hand-written and drifts (dots record docs/forth.md stating the opposite of what ships and plan documents contradicting the tree), nothing generates a library reference from the typed signatures the checker holds, and the idiomatic corpus is the library plus Tender. A model learns from what the tree publishes. Acceptance: a library reference generated from public signatures and package doc comments, byte-identical across two builds of one tree, with a gate that fails when a public signature changes without its doc; docs/forth.md, docs/stdlib.md, PLAN.md and LESSONS.md verified against the tree; LESSONS entries are rules, not narratives. Children (open): habu-generate-the-lib-79a7e837 habu-scan-public-cast-95c07c25 habu-correct-stale-type-da9b9f3d habu-lessons-md-is-6c14783f habu-make-each-native-0c44958a . Absorbed on 2026-09-16: 145 dots closed with the reason 'superseded by habu-campaign-c7-learning-b18a8259'; find their text with dot find. Files: tools/public-signatures.f, tools/ (new docgen tool), docs/, LESSONS.md, docs/roadmap.md section C7. Verify: the docgen gate; docs review. Depends: none. Ownership: docs lane. Claim: unassigned. Absorbed: see the archive entries closed with 'superseded by' this id.
