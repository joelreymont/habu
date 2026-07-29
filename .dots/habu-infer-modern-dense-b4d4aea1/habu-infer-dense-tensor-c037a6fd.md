---
title: Bind Qwen tensor catalog
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.441007+02:00"
blocks:
  - habu-infer-dense-pin-36c8e45c
  - habu-parse-qwen-config-cd65019c
---

Why: the pinned checkpoint needs one exact role-to-slot authority before shard loading. Result: package QWENTENSOR owns checked construction of exactly 339 roles, fixed slot lookup, inverse slot lookup, and the catalog census. Separate leaves own name conversion and layout validation. Add no second configuration, dtype code, generic model role, string-key runtime lookup, packed layout, optional tensor, tied-head assumption, compatibility alias, or pack catalog. Owner: QWENTENSOR role construction and slot bijection only. Production red: no catalog can address the 339 pinned roles. Acceptance: every role maps to one slot and every slot maps back; missing, duplicate, extra, wrong layer, and one-over roles reject; mutating any role fails the focused census. Smallest owning check: bin/hb --load maki/infer/qwen-tensor-test.f. Claim: unassigned.
