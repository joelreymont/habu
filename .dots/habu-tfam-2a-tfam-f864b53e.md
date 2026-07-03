---
title: "TFAM 2a: TFAM/SUMV/SCHEMA registries + snapshot"
status: active
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-03T23:36:48.911831+02:00\\\"\""
---

PLAN.md item 2 (registry half). Growable checker-owned registries for families/variants/product fields/layouts/schemas at src/core/type-family.f + type-schema.f (new files; update tools/srclist.f, FILEMAP.md, build-cache keys). Records: package id, visibility, lowercase tail, arity, kind, layout policy, slots, variant range, tag width, schema roots, span. Reject uppercase/mixed-case tails at declaration. Fixtures: add/find, qualified lookup, visibility, dup rejection, same-tail cross-package, grow, snapshot persist/restore. Gate 17b. Depends: TFAM 1.
