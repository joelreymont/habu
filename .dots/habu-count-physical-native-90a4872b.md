---
title: Count physical native call sites across aliases
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-24T17:41:33.816706+02:00\""
---

Problem: EXPORT aliases create multiple live dictionary records over one machine-code span, so NREACH:REDIRECT returned its duplicate-record preflight count instead of the physical instructions it rewrote. Acceptance: through the real native-reach load path, an exported caller alias whose code contains two BL instructions makes REDIRECT return two; every refusal and site preflight still completes before the first patch32 write. Files: src/compiler/native/reach.f and test/compiler/native-reach.f. Verify: old-tree production fixture proves BLS-IN=2 and REDIRECT=4; fixed native-reach owner suite; forced fixpoint refresh; maki/test.f; test/run.f; error-code-lint and private-root dot-dep-lint. Depends: none. Ownership: native redirect write-count semantics and its regression only; no scanner, codewalk, workload, or predecessor-publication changes.
