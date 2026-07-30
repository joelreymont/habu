---
title: Key declaration sources
status: active
priority: 2
issue-type: task
created-at: "2026-07-21T06:59:26.054283+02:00"
---

Why: `BF-APPEND-DECL-FILES` already builds `src/core/decl-event.f`,
`src/core/structure-make.f`, `src/core/structure-decl.f`, and
`src/core/enum-decl.f`, but `TR-UNDER-SOURCE-FILES` omits them. The candidate
cache can therefore reuse an engine built from stale declaration code.

Result: add exactly those four existing paths to `TR-UNDER-SOURCE-FILES` in
their current build order. Change no build list, manifest, role, lint, source
loader, or declaration code. Owner: `test/run-files.f` candidate source key
only. Production red: with a warm candidate stamp, changing any one of the four
files does not change `TR-UNDER-SOURCE-KEY` and the gate reports a cache hit.
Acceptance: changing each file changes the key and forces a candidate rebuild;
an unchanged tree remains a cache hit; generated engine bytes and declaration
load order stay exact. Forbidden: a new manifest role, walker, lint, fixture
parser, duplicate build path, cache bypass, or unrelated source-key cleanup.
Smallest owning check: the existing run-result-cache key test plus one warm-cache
mutation probe for each of the four paths. Claim: agent=decl-key-impl
workspace=.jj-ws/habu-add-decl-event-a50e4104-r2.
