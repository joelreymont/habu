---
title: Retire target-selecting host predicates
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:29.035141+03:00"
---

Problem: 74 HB-TARGET-* uses select target semantics on the building engine (compiler.f:55-63, native/abi.f:31-41, native-runtime.f:34-44, 118-126, layout.f:78-84, arm64/machine.f:83-85, aot-*.f, build tools); the other ~340 select host services and stay (PA-r2 §5.3, §15.1, P13). Acceptance: the 74 read the resolved target; a lint refuses a new target-semantic caller; file moves to src/arch/arm64/ land as separate commits with byte parity; no hidden fallback. Files: the 74 sites, tools/dep-lint (new rule), src/arch/arm64/. Verify: full gate; byte-identical ARM64 and x86-64 images before and after each move. Depends: habu-resolve-build-targets-ae8e65c1, habu-give-each-backend-b6f7ea4f, habu-link-native-fragments-5949ec30. Ownership: moved files by agreement with Heron and the Intel lane. Lane: dave. Claim: unassigned.
