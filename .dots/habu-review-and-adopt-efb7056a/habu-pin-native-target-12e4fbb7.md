---
title: Pin native target digests and primitive goldens
status: closed
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.775992+03:00"
closed-at: "2026-10-03T23:19:43.667390+03:00"
close-reason: Literal schema-1 preimages and SHA-256 pins for three native bindings and PTX sample; native layout pins and existing primitive parity retained. Independent Astra PASS; frozen combined native gate 591/591 exit0. Private FP wire-code mutation builds and fails all four preimage/hash pins; native layouts and target-policy focused checks exit0. Intel execution untested.
---

Problem: test/compiler/target-policy.f:39-48 pins preimage slot codes and domain counts, no literal CTARGET digests, so a target.f edit cannot prove legacy digests unchanged (PA-r2 P0, §4.2). Acceptance: literal SHA-256 digests and preimages for aarch64-darwin, aarch64-linux, sysv-amd64 and the sample contract; primitive parity and layout goldens recorded; a changed digest fails the suite. Files: test/compiler/target-policy.f, test/compiler/target-digest-goldens.f (new), test/gate-stdlib-cases.f. Verify: bin/hb --load test/run.f; the new suite fails when a wire code is renumbered (both-direction proof). Depends: none. Ownership: test/compiler/target-*.f. Lane: dave. Claim: dave.
