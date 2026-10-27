---
title: Delete the artifact and whole-app object caches
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T13:24:03.166400+02:00"
---

Problem: see parent. Change: Lane 3: hb-build-lib artifact copy, AOT inputs, object load/store and engine artifact deleted; engine selection becomes the support image's sidecar; src/lib/object*.f, tools/object-image*.f, retain and aot-cache tests and E-OBJ-* deleted; --report-json drops cache_root, cache_source, artifact_hit, object_hit with no version change. Design: /Users/joel/.cache/tmp/carl-file/design-build-out.md §5. Verify: the lane's proof list in the design. Depends: previous lane. Ownership: carl. Claim: unassigned.
