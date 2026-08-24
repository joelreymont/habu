---
title: Count physical native call sites across aliases
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-24T17:41:33.816706+02:00\""
---

Problem: EXPORT aliases create multiple live dictionary records over one machine-code span, so NREACH:REDIRECT and CODEGEN-SCAN:CALL-SITES count the same physical BL instructions once per alias. Acceptance: through the real native reach and workload-scan load paths, an exported caller alias with two BL sites reports two scanner sites and REDIRECT returns two, while every reach refusal/preflight still completes before any patch32 write. Files: src/compiler/native/codewalk.f, src/compiler/native/reach.f if owner semantics require it, test/compiler/native-reach.f, tools/codegen-workload-scan.f, tools/codegen-workload-test.f, and LESSONS.md only for a new durable lesson. Verify: old-tree production fixture proves BLS-IN=2 and REDIRECT=4; fixed native-reach and codegen-workload owner suites; forced fixpoint refresh; maki/test.f; test/run.f; error-code-lint and private-root dot-dep-lint. Depends: none. Ownership: native physical-code walk alias handling and its reach/workload regressions only; no predecessor publication.
