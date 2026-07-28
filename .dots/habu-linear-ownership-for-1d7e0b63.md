---
title: Linear ownership for compiler context handles
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T18:05:11.520876+02:00"
---

Full context: IR-CTX enforces context/handle ownership with a generation-token registry plus fail-closed lifecycle checks — the interim mechanism design docs/compiler-ir-design.md section 6.2 sanctions until linear types can own it. Replace it with DEFLINEAR-style linear context (and later builder) tokens per the type-habu epic (habu-epic-type-habu-a34713f0), which retires the bounded top-level slot retirement after out-of-context throws and the DEPTH-MAX 64 ceiling outright. Acceptance: the linear token API compiles the existing test/compiler/ir-context.f acceptance suite unchanged or with mechanical renames; the registry words are deleted, not kept as a fallback.
