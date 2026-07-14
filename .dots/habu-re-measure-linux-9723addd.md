---
title: Re-measure Linux build-size ratchet on Orin
status: open
priority: 2
issue-type: task
created-at: "2026-07-14T16:09:54.564839+02:00"
---

test/gate-build-size.f:40 GB-SIZE-BASELINE-LINUX is 102592 (fable re-measure 2026-07-04) but current master's Linux-aarch64 bin/hb is 147648 (post type-family checker + session landings; fixpoint x2 byte-identical sha 3d714be6...), so test/run.f on the Orin is RED in both native engine build + candidate validation slices: 'FAIL: candidate size ratchet: grew past test/gate-build-size.f baseline'. macOS baseline 165367 unaffected. Fix: re-measure on current master on the Orin, bump the constant with a dated comment, prove the build-size gate green on-device (zed ~/Work/habu synced to the same master). Growth cause is legitimate landed capability, not a leak - but confirm with the size-gate breakdown before bumping. Files: test/gate-build-size.f. Verify: on-device test/run.f build slices green. Depends: none (zed provisioned). Ownership: build-size gate constants.
