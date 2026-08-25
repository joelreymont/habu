---
title: schedule or delete the eight dark tool tests
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.148857+02:00"
---

Problem: tools/seed-test.f (the trusted-seed installer's only test), tools/json-test.f, tools/diff-side-content-test.f, tools/hb-open-failure-test.f, tools/stdlib-time-test.f, tools/zed-run-test.f, tools/ptx/gpt2-attention-device-test.f, tools/ptx/gpt2-tensor-device-test.f are referenced by no runner (repo-wide path-reference count 0); tools/lint/schedule-lint.f:58-61 walks test/ only by design, so the dark-test class is open for the whole tools tree (owed by open dot habu-tier-parity-lint-31aaa95b). Acceptance: DISK-WALK covers tools/ *-test.f (the pragma mechanism at :86-93 exists); the eight scheduled, marked manual with a recorded run, or deleted (zed-run* is a remote-ssh harness for a dead host). Files: tools/lint/schedule-lint.f, the eight files. Verify: schedule-lint 0 unreached over tools/. Depends: habu-tier-parity-lint-31aaa95b. Ownership: lints. Claim: unassigned.
