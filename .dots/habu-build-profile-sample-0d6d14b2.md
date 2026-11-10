---
title: "Build profile: sample finely enough to rank a 1% row"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T11:04:38.307992+02:00"
---

Problem: tools/build-profile.f MAIN starts '0 prof-on' at the 1 ms default (src/habu/prof.f:716), so a 1.5 s warm engine build gives about 1,400 samples: a 3% row holds about 40 and a 1% row about 14, too few to rank rows or to compare a row before and after a fix. prof-rate already sets the interval, and a tick costs about 1 us of CPU (prof.f:5-6, measured at 20 us). Acceptance: build-profile.f sets '100 prof-rate' before prof-on, with no new argument: about 1% overhead and about 14,000 samples for a warm build; the report header reads usec 100 and dropped 0; docs/debugging.md 'Profiling the build' states the rate and what count a row needs before it is read. Files: tools/build-profile.f, docs/debugging.md. Verify: a warm engine build's profile header shows usec 100, samples above 10,000 and dropped 0; the build's wall time under the profiler is within 5% of the 1 ms run. Depends: none. Ownership: tools/build-profile.f. Claim: unassigned.
