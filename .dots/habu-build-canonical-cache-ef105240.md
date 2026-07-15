---
title: "Build: canonical cache root"
status: active
priority: 1
issue-type: task
created-at: "2026-07-15T12:47:32.768142+02:00"
---

Full context: hb-build only honors explicit HABU_BUILD_CACHE, while the framed-diff producer needs a persistent content-keyed scanner build cache without duplicating environment/default policy. Add one checked build-cache root resolver package consumed by hb-build and other build clients. Exact precedence: explicit HABU_BUILD_CACHE, XDG_CACHE_HOME/habu-build, HOME/.cache/habu-build, TMPDIR/habu-build. Return a typed source classification plus path; create missing directories, reject an existing non-directory or unwritable root, never silently switch tiers after a selected tier fails, and expose the selected root/cache source in diagnostics. Add a structured hb-build report containing the selected root/source, artifact/object/maker hit flags and elapsed time; cache clients consume this report instead of inferring hits from latency or child-private state. Add the checked FS writable-root predicate needed by the resolver. Migrate hb-build to this owner without changing explicit-env semantics. Add fixtures for every tier, empty variables, spaces and adversarial bytes, selected-tier failure/no-fallback, concurrent creation, cache hit persistence, report truthfulness and writable-root rejection. Update docs/manifests/FILEMAP. Blocks framed diff scanner integration. No scanner or diff parser edits. Claim: agent=build-cache-root workspace=.jj-ws/habu-build-canonical-cache-ef105240.
