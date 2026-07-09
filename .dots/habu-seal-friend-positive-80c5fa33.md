---
title: "Seal: friend-positive fixture when reserved system packages become real"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T17:59:44.278592+02:00"
---

From TFAM 2b-ii (landed): the friend branch (latch 0 allows package TFAM/TYPE/MATCH during engine cold load) is proven only structurally today because no reserved system package is actually created during cold load yet. When the type-family publishing slice (item 6+/8) creates real TFAM/TYPE/MATCH system packages friend-side, add a positive fixture to test/seal-package.f proving a TFAM-qualified word resolves in user source post-seal (use without create), alongside the existing negatives. Owner: whichever item first populates a reserved system package.
