---
title: Make engine-size sum install-class images exactly
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T20:54:47.165843+03:00"
---

Problem: tools/engine-size.f sums an install-class image (one carrying an aot/checker-sidecar section) 8 bytes short of the file size and exits 74 there (private-words lane, 2026-09-16); it is exact on native-runtime images, so every published budget number is from those. Acceptance: the section walk accounts for the sidecar's header or alignment so the sum equals the file size on both image classes, with a fixture in tools/engine-size-test.f (or the tool's existing test) that builds or ships one install-class image and checks the sum; exit code only on a real mismatch. Files: tools/engine-size.f, its test. Verify: tools/engine-size.f on a native-runtime engine and on a build-fixpoint --install engine, both exit 0. Depends: none. Ownership: tools/engine-size.f. Claim: unassigned.
