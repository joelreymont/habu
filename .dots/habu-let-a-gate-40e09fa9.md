---
title: Let a gate file report every red case, not the first
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T13:16:25.192358+03:00"
---

Problem: the gate files under test/ that drive child programs through lib/test/runner.f (test/gate-dictionary.f and siblings) stop at their first FAIL, so a red gate hides its later cases: two cases in test/gate-dictionary-lib.f were red behind the first failure until 2026-09-12. Acceptance: a gate file runs every case, prints each FAIL, and exits nonzero at the end (the shape test/run.f now has); the runner's die-at-first-fail stays available to callers that want it by name. Files: lib/test/runner.f, test/gate-common.f, the gate files. Verify: a gate with two injected failures reports both; test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
