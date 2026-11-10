---
title: "Cut file-objects case N's three fresh builds"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T13:00:24.268970+02:00"
---

Problem: test/file-objects.f NEST-CASE runs tools/hb-build.f three times in fresh hb processes and its program twice; file-objects went from 14.4 s to 23.8 s wall. The boundary under test is the object another build wrote, not the process, and each hb-build repeats the kernel build. Acceptance: case N's assertions unchanged (first run prints 1, last prints 2, the stale link fails it) with the repeated setup removed, or a measurement showing where the 9 s goes and that none of it repeats. Files: test/file-objects.f, a driver under test/fixtures/file-objects/ if one is needed. Verify: file-objects wall and CPU before and after. Depends: habu-name-every-piece-da16d296 and habu-list-the-words-9c7141f7 merged (same file). Ownership: unassigned. Claim: unassigned.

Brief: carl-layout/.out/briefs/lane-case-n-cost.md. Claim: agent=carl workspace=.jj-ws.noindex/carl-words (carl-ncost retired).

Merged 2026-11-10 into the stack as zolomuqz da10c376 "Build file-objects' case N in L's out root" (test/file-objects.f only): N builds in L's out root after L's build, which leaves the kernel's objects there. Worker's measurement: file-objects 25.6 s -> 17.9 s wall, CPU 19.8 s -> 13.5 s, rc 0 PASS; r.f in an empty root 8.0 s, each of N's builds in L's root 0.95 s. Closes when the feature lands.
