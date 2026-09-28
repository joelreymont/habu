---
title: Share buffer bounds logic
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-28T18:51:17.431465+02:00\\\"\""
closed-at: "2026-09-28T21:52:26.254344+02:00"
close-reason: Share generated bounds and numeric offsets through checked helpers while preserving typed bases, negative-first refusal, current capacity and fresh mapping reads. Actual197 fixed accessors80→52B and159 dynamic112→64B, helpers196B, generator source bodies-192B; captured code-13144B. Native B1/B2/names match, storage/capture focused checks pass. Complete signed engine2477047→2460535B (-16512) including all metadata/padding/signatures. Independent review passes; final three-feature chain dc3bea8d passes five generations/names,492/492 suites and Maki actual-stdin board equality. No standalone full gate claimed. Lead verified exact source/artifact hashes and qualification receipt ~/.cache/tmp/habu-source-sharing-completion-20260928-01.md.
---

Exact product census found 197 typed-buffer accessors and 159 dynamic-buffer accessors, with 20268 bytes of repeated bounds and pointer logic. Factor only checked scalar range/offset computation into existing owner helpers; preserve typed generated bases, relocation carriers, public effects, error7122, negative-first checks, current capacity after growth and pointer refresh. Reserve/release already share helpers. Measure wrapper, helper, metadata and full signed-file costs; reject nonpositive result. Existing real-load storage/dynamic capture tests, genuine gaps before code, independent review and native/full-registry/Maki qualification. Evidence: ~/.cache/tmp/habu-repeat-source-design-20260928-01.md and habu-generator-census-completion-20260928-01.md.
