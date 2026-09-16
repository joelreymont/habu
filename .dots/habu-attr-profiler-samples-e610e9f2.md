---
title: Attribute profiler samples in words compiled after prof-on
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-16T13:08:16.448507+03:00\""
---

Problem: the pc-sorted attribution index (slice 1 of habu-build-the-internal-4cd07a82, commit 285c3541 in .jj-ws/hazel-profiler, verified on one build but not yet landed) is built at prof-on, so a word compiled after prof-on is counted in a distinct (new) bucket instead of by name. For the self-build that is most of the profile: the native compiler's hot words (IR-ARENA:RD@ and friends) are required during the build, after prof-on. Decision needed, then implementation: deferred attribution (the handler buffers pc and caller pcs, the report attributes after rebuilding the index when every word exists) or an incremental index extension outside the handler (the handler must never read a record under construction). Acceptance: profiling tools/native-build.f end to end names the compiler's own words with no (new) bucket above 1 percent, handler cost stays under 2 us, the property test of slice 1 still holds. Files: src/habu/prof.f, test/prof-index.f. Verify: the self-build profile and the fixture. Depends: habu-build-the-internal-4cd07a82. Ownership: hazel line. Claim: agent=hazel-profiler workspace=.jj-ws/hazel-profiler.
