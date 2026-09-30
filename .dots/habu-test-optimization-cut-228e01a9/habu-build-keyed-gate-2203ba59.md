---
title: Build keyed gate images in pool rows
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T15:12:49.909738+02:00"
closed-at: "2026-09-30T15:13:06.425907+02:00"
close-reason: "Landed 39e7d7e8 (snapshot-writer conflict resolved onto master's BUILD-LOADING-TO). Fable review accepted; closure-edge mutations red as specified. Cold-gate 65.6 s serial setup moved into pool rows. Native gate 504/504 rc 0, 281.2 s wall, 1984 s pooled at load 78-105 (gated at d4d16008, pushed as d8867baf after rebasing over a dots-only commit)."
---

Problem: the native gate built its keyed images (fixture writer, cold engine, app image, linker, whitebox) serially in SUITE-SETUP, so a cold gate spent 65.6 s before its first suite and one failed image build stopped the whole gate. Acceptance: each image family is a pool build row (test/gate-images.f); a row starts once the images its load closure needs are settled; a failed build reds only the rows that need it, naming the build row; a row run alone settles its own images. Verify: mutations of the closure edges (dropped edge exits 69 not granted); full native gate cold and warm.
