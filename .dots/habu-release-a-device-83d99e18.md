---
title: Release a device row whose close operation throws
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T21:47:00.377429+03:00"
---

Problem: lib/genio.f CLOSE (7cf8a12a, 2026-09-16) moves a row LIVE -> CLOSING by compare-and-swap, runs the device's close operation, and only then RELEASE-ROW; a close operation that throws (TCP-CLOSE answers E-GENIO-IO when TCP4:CLOSE fails) leaves the row in CLOSING for the life of the process: not live, so every handle refuses, and not free, so no DEVICE can take it, and RESET-ROUTING at capture is the only thing that clears it. Acceptance: the row is released whether the close operation returns or throws (catch, release, rethrow), the generation still moves so the old handle refuses, and a test in lib/genio-test.f closes a device whose close operation throws and then opens DEVICES devices successfully. Files: lib/genio.f, lib/genio-test.f. Verify: lib/genio-test.f; SUITE genio. Depends: none. Ownership: lib/genio.f (aspen's module). Claim: unassigned.
