---
title: Remove redundant AOT source lint
status: closed
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.120834+03:00"
closed-at: "2026-10-16T12:20:23.131684+03:00"
close-reason: Removed redundant lexical AOT lint and its CLI subprocess plumbing. Actual closure refusals remain; unused patch32 builds, reachable trusted patch32 refuses. AOT positive/negative/cache, standalone lint and full hb-build CLI suites pass; independent review clear.
---

Problem: tools/aot-lint-core.f rejects five token spellings anywhere in source before hb-build reaches the actual AOT closure. That rejects unused words and spellings even though src/habu/aot-closure.f already checks reachable runtime calls. Remove the lexical lint, its child/direct hook and wrapper, and exclusive tests and registration. Remove unused CLI-CHECK helpers. Keep the closure's E-AOT-UNSUPPORTED diagnostics, source locations, and valid build/cache behavior. Verify focused hb-build and AOT closure paths, including a build with an unreachable unsupported call and a reachable-call refusal. Ownership: AOT lint files, tools/cli-run.f, relevant tests/docs, and only lint plumbing in tools/hb-build-lib.f. Claim: fix_aot_lint.
