---
title: Canonicalize saved builder identity paths
status: closed
priority: 2
issue-type: task
created-at: "2026-10-21T03:55:46.415144+03:00"
closed-at: "2026-10-21T10:03:24.984053+03:00"
close-reason: Canonicalize saved-builder executable and selected sibling-map identity while preserving lexical donor selection and semantic source roots. Relative and absolute invocations reuse one saved maker, both relink and run identical executable/map pairs; symlink-selected missing-map attribution and map restoration are covered. AOT image, stripped cache, AOT cache/producer/codesign, and observed-input rows pass. Independent Astra review clear for all changed paths; source-only tool change, no runtime rebuild required.
---

While qualifying ordinary hb-build test sharing, the same saved linker engine reached HBB-MAKER-KEY-COLD through relative and absolute path spellings. CONTENT-KEY:FILE+ folds its filename, so identical builder products appeared under two producer keys (73ef748 and bdb734; SHA25633380af1 prefix). Absolute-root gate rows reuse the same two builder inodes; this is extra setup cost for relative cache roots, not observed corruption. Evidence will be retained in the carl-hbt-sharing measurement bundle. After current suite-time work, settle canonical engine-path acquisition at the responsible identity boundary without dropping semantic source-root/input-path distinctions, then verify real relative/absolute invocation reuse and equal products. Do not add alias caches, fallback keys or duplicate validation.
