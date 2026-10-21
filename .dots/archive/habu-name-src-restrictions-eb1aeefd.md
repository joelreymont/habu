---
title: Name source restrictions vocabulary
status: closed
priority: 2
issue-type: task
created-at: "2026-10-21T10:24:08.533825+03:00"
closed-at: "2026-10-21T10:29:03.809527+03:00"
close-reason: Renamed public module/package to src/lib/vocabulary.f/VOCABULARY with ALLOW-PACKAGE, RESTRICT, SOURCE-ROOT!, WITH-SOURCE-ROOT. Updated Habu callers, paired tests, registry, docs and open task references; no old API aliases. Existing two-tier/root/refusal suite141cases and saved-image writer row both pass on qualifieda30 runtime, with repeatable vocabulary-run and policy-image artifacts. Baked-file edits are comments only; runtime primitive/error ABI unchanged. Independent Astra review and omission follow-up clear.
---

Rename src/lib/policy.f and package POLICY to src/lib/vocabulary.f and VOCABULARY. Public operations become ALLOW-PACKAGE, RESTRICT, SOURCE-ROOT!, WITH-SOURCE-ROOT; update Habu callers, tests and docs without changing the runtime restriction behavior or engine ABI. Verify the existing two-tier restriction/load/root cases and affected saved-image path. Keep separate from the paused incremental-build work.
