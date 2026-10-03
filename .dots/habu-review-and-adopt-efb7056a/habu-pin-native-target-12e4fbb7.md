---
title: Pin native target digests and primitive goldens
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.775992+03:00"
---

Problem: test/compiler/target-policy.f:39-48 pins preimage slot codes and domain counts, no literal CTARGET digests, so a target.f edit cannot prove legacy digests unchanged (PA-r2 P0, §4.2). Acceptance: literal SHA-256 digests and preimages for aarch64-darwin, aarch64-linux, sysv-amd64 and the sample contract; primitive parity and layout goldens recorded; a changed digest fails the suite. Files: test/compiler/target-policy.f, test/compiler/target-digest-goldens.f (new), test/gate-stdlib-cases.f. Verify: bin/hb --load test/run.f; the new suite fails when a wire code is renumbered (both-direction proof). Depends: none. Ownership: test/compiler/target-*.f. Lane: dave. Claim: unassigned.
