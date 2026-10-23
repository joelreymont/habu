---
title: Recompile a file whose replay goes stale (L3b)
status: active
priority: 2
issue-type: task
created-at: "2026-10-24T00:02:39.462142+03:00"
---

A replay that refuses after INSTALL-BEGIN (stale later segment, NAMED-STEP install refusal) throws E-NUNIT-PROFILE and fails the build; it must roll back to the file's start and compile from source. Blocks landing: tools/hb-build-stripped-cache-test.f fails on unchanged sources (ptrcell then maplate; aot-decl.f segment 2 resolves the app's global BUF; findings /Users/joel/.cache/tmp/carl-file/candid-handoff.md). Same cause: frame TFAIL 315/316, dynamic-load 27/29/30/34. Design: Plan (Fable) running; output to /Users/joel/.cache/tmp/carl-file/design-l3b.md. Lanes branch from ynxllvlk in .jj-ws.noindex/carl-hit.

Ruling: process restart (R) with a build-scoped failed-key exclusion; driver retries the maker. Full checkpoint rollback and resume-from-source rejected. Design and lanes: /Users/joel/.cache/tmp/carl-file/design-l3b.md (lead ruling at the end). ynxllvlk review: ACCEPT after a one-line fold (rows past the count are cleared, not BAD).
