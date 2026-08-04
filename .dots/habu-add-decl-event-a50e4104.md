---
title: Key declaration sources
status: closed
priority: 2
issue-type: task
created-at: "2026-07-21T06:59:26.054283+02:00"
closed-at: "2026-07-30T09:08:10.735408+02:00"
close-reason: Implemented in 06ac2e78ba4f369ea09f8e60cd1102bdd06db8a5; independently reviewed and landed on master@origin; focused production cache proof, typed-local, package, Maki, PTX standard-library, host, and dot gates green.
---

Claim: RELEASED 2026-07-29 by the stale-claim audit. Agent `decl_event_manifest` and workspace `.jj-ws/habu-add-decl-event-a50e4104` are both gone: the directory does not exist and `jj workspace list` has no record of it. The work has not landed - `tools/build-fixpoint.f` still hardcodes the four declaration source literals in `BF-APPEND-DECL-FILES`. The dot stays active and is free to claim.
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
