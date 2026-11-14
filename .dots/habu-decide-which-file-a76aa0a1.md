---
title: Sync every published file to disk
status: active
priority: 2
issue-type: task
created-at: "2026-11-05T13:20:20.006579+02:00"
---

Problem: FS:ATOMIC-WRITE-FILE syncs the sibling and the directory, and the build's objects go through it (tools/hb-build-core.f:84, :351, :362). The other publishers rename with no sync, so a crash can leave an installed hb or a built image empty under its name (fsync review findings 5 and 6): FS:REPLACE-STAGED (src/lib/fs-mutate.f:1051; callers tools/hb-build-lib.f:234, src/lib/svg-pdf.f:168, :174, src/habu/link-map.f:353, src/habu/link-data.f:546, src/habu/linker.f:2621); tools/install.f PUBLISH (:150-152: FS:COPY-FILE-STREAM into a new tree, then a link renamed over ~/.local/lib/habu); tools/bundle-lib-core.f:316 and src/lib/zip-raw.f:613 (FS:RENAME-FILE of a temporary); tools/seed.f:122 through CODESIGN:PROMOTE-EXECUTABLE (src/lib/codesign.f:382; PROMOTE-SIGNED-EXECUTABLE :389). A cold spark engine build costs the same with the sync and without it.

Approach: FS:REPLACE-STAGED syncs the sibling after its fill and the directory after the rename; bundle-lib-core, zip-raw and seed publish through it, and CODESIGN:PROMOTE-EXECUTABLE and PROMOTE-SIGNED-EXECUTABLE go; install.f syncs the new tree's files and the link's directory before and after its rename; ATOMIC-WRITE-FILE keeps its sync.

Acceptance: every rename that publishes a written file follows a sync of that file and is followed by a sync of its directory; rg finds FS:RENAME-FILE of a written file only inside FS:REPLACE-STAGED and FS:ATOMIC-WRITE-FILE; rg -i --no-ignore finds no PROMOTE-EXECUTABLE; the fs-mutate, codesign, install-e2e and zip suites pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-sync
