---
title: Repair process outcome consumer gate
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T05:27:01.533087+02:00"
---

Problem: test/load-reject-diag-test.f still expects four scalar results from RUN-ARGV-STDIN-CAPTURE-OUTCOME, but the process API now returns OUTCOME, so the checked load fails at LRD-STORE! and master test/gate-stdlib.f is red. Cause: consumer migration was omitted during the OUTCOME cutover. Fix: migrate the fixture through exhaustive OUTCOME MATCH helpers while preserving exit kind, code, stdout length, stderr length, and named-rejection assertions; audit every RUN-ARGV-STDIN-CAPTURE-OUTCOME consumer for the same stale scalar effect. Acceptance: the focused fixture, ptx-stdlib gate, full stdlib gate, maki suite, host-lint, filemap-lint, and dot-dep-lint pass. Files: test/load-reject-diag-test.f and any proven stale direct consumer tests only. Verify: bin/hb --load test/load-reject-diag-test.f; bin/hb --load test/gate-stdlib.f. Claim: unassigned.
