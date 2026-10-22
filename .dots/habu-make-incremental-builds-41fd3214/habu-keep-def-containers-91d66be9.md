---
title: Keep definition containers in the selected cache root
status: open
priority: 1
issue-type: task
created-at: "2026-10-22T12:18:15.952118+03:00"
---

Problem: tools/native-definition-cache.f:399-402 FILE-ROOT$ reads HABU_BUILD_CACHE raw and falls back to <cwd>/.out, while artifacts use the BUILD-CACHE tiers (src/lib/build-cache.f); hb-build never exports its resolved root to maker/support children (tools/hb-build-lib.f:712-723, :1466-1470). Without the variable a one-line app puts ~110 MB of hb-def-*.hbd under <cwd>/.out. No BUILD-CACHE:USED/PRUNE covers .hbd, so the largest family is never retained or pruned. docs/native-applications.md:86-87 claims both live in HABU_BUILD_CACHE. Also src/lib/build-input.f:3-4 comment is stale (stripped caches key on the actual rows digest). Acceptance: one root resolved once via BUILD-CACHE:ROOT$ and exported to children; containers dated and pruned like other families; no .out fallback; E2E with the variable unset writes nothing under cwd. Files: tools/native-definition-cache.f, tools/hb-build-lib.f, src/lib/build-input.f. Depends: none. Claim: unassigned.
