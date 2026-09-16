---
title: Reload an engine-provided file whose source changed
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:02:00.203290+03:00"
---

Problem: lib/errors.f is engine-provided (src/habu/native-runtime.f requires it, the cold prefix carries it via tools/build-fixpoint.f BF-APPEND-ERRORS), so `require lib/errors.f` in an application is a no-op and a constant added to the on-disk file is invisible until the engine is rebuilt (measured 2026-09-16 by aspen's semaphore worker: three new codes undefined on /tmp/hazel-fH2; it had to inject them to test). Every library lane that mints an error code therefore needs its own engine build, and a stale root bin/hb silently loses new codes. The same holds for every engine-provided file (src/core/*, lib/errors.f, whatever ENGINE-PROVIDES? answers yes for). Acceptance: decide and implement one rule in src/core/include.f: either (a) a provided row records a content digest at capture and RESOLVE reloads the file when the on-disk digest differs, refusing with a named error where reloading is impossible (checker/loader core), or (b) the loader refuses by name (E-ENGINE-STALE) when a provided file's on-disk content differs from the captured one, so staleness is loud instead of silent; docs/bootstrap.md states the rule; a regression edits a copy of lib/errors.f in a scratch tree and shows the chosen behaviour; build-fixpoint and stripped builds unaffected. Files: src/core/include.f (ENGINE-KNOWN?/RESOLVE/REQUIRE-STORE), src/habu/native-runtime.f, tools/build-fixpoint.f, docs/bootstrap.md, test/require-cap-test.f or a new fixture. Verify: the regression; tools/native-build.f fixpoint; bin/hb --load test/run.f. Depends: none. Ownership: loader. Claim: unassigned. Source: aspen 2026-09-16 16:30.
