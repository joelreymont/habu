---
title: Resolve build targets through one action model
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.806657+03:00"
---

Problem: tools/build-target.f holds one ambient cell and src/compiler/native/abi.f:31-41 derives the host contract from HB-TARGET-* predicates; there is no ResolvedTarget, alias resolver or separate compatibility predicates (PA-r2 §2-§4, P1b). Acceptance: ExecutionPlatform, CompilerProduct, ResolvedTarget and SameBuildIdentity/LinkCompatible/RuntimeAdmissible/ExecutableHere as checked records under a new package; existing --target labels resolve as aliases; T01-T06 and T15 pass; legacy CTARGET digests unchanged. Files: src/compiler/target/ (new), tools/build-target.f, tools/native-build-args.f, src/os/*/target.f, src/compiler/native/abi.f (host-descriptor split). Verify: full gate; test/compiler/target-resolve.f (new, registered). Depends: habu-add-the-wasm-4c32353e. Ownership: src/compiler/target/, tools/build-target.f. Lane: dave. Claim: unassigned.
