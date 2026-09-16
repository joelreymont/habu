---
title: Attribute profiler samples in words compiled after prof-on
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-16T13:08:16.448507+03:00\""
---

Problem: the pc-sorted attribution index (slice 1 of habu-build-the-internal-4cd07a82, commit 285c3541 in .jj-ws/hazel-profiler, verified on one build but not yet landed) is built at prof-on, so a word compiled after prof-on is counted in a distinct (new) bucket instead of by name. For the self-build that is most of the profile: the native compiler's hot words (IR-ARENA:RD@ and friends) are required during the build, after prof-on. Decision needed, then implementation: deferred attribution (the handler buffers pc and caller pcs, the report attributes after rebuilding the index when every word exists) or an incremental index extension outside the handler (the handler must never read a record under construction). Acceptance: profiling tools/native-build.f end to end names the compiler's own words with no (new) bucket above 1 percent, handler cost stays under 2 us, the property test of slice 1 still holds. Files: src/habu/prof.f, test/prof-index.f. Verify: the self-build profile and the fixture. Depends: habu-build-the-internal-4cd07a82. Ownership: hazel line. Claim: agent=hazel-profiler workspace=.jj-ws/hazel-profiler.
Landed 2026-09-16 19:45 (duplicates of b433a507, ff491807, 3985bbae, bb423eb1): deferred attribution for words compiled after prof-on ((new) 15.2 percent to 0.06 percent on the self-build, identity exact), perf-map tool, every word in prof-json with a flat edges array and prof-row for any record, tools/build-profile.f (Joel's directive), clobber fixed; nine new gate cases; prof-index and perf-map suites registered. Closes with habu-build-the-internal-4cd07a82 and habu-profile-the-build-c26f424c on the next batch gate.
