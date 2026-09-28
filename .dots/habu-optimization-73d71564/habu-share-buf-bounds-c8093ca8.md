---
title: Share buffer bounds logic
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-28T18:51:17.431465+02:00\""
---

Exact product census found 197 typed-buffer accessors and 159 dynamic-buffer accessors, with 20268 bytes of repeated bounds and pointer logic. Factor only checked scalar range/offset computation into existing owner helpers; preserve typed generated bases, relocation carriers, public effects, error7122, negative-first checks, current capacity after growth and pointer refresh. Reserve/release already share helpers. Measure wrapper, helper, metadata and full signed-file costs; reject nonpositive result. Existing real-load storage/dynamic capture tests, genuine gaps before code, independent review and native/full-registry/Maki qualification. Evidence: ~/.cache/tmp/habu-repeat-source-design-20260928-01.md and habu-generator-census-completion-20260928-01.md.
