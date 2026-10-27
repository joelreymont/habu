---
title: Delete the build cache module
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T13:24:03.184142+02:00"
---

Problem: see parent. Change: Lane 4: src/lib/build-cache.f, fs-lock.f, their tests and tools/build-cache-probe.f deleted; build-support, gate-stdlib-lib, two-generation, build-fixpoint and cli-errors stop exporting HABU_BUILD_CACHE; .gitignore gains .out/; docs/gate.md, bootstrap.md, architecture.md state the per-tree layout once. Design: /Users/joel/.cache/tmp/carl-file/design-build-out.md §5. Verify: the lane's proof list in the design. Depends: previous lane. Ownership: carl. Claim: unassigned.
