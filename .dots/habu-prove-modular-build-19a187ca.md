---
title: Prove modular-build milestone
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T13:04:59.999988+02:00"
blocks:
  - habu-bind-cache-valid-3b6d1aba
---

Fan-in: canonical cache already on master; habu-compile-authenticated-src-05e058a2, habu-remove-synthetic-compose-373b117a, habu-build-exact-modular-44f4c2dc, and habu-bind-cache-valid-3b6d1aba. Prove direct versus built behavior/failure identity for every parser/frame EOF class, initial provided registry, repeated include/require, mutation after freeze, cache cold/hit/miss truth, and exact diagnostics. Run native/recovery/AOT/snapshot/fixpoint/hb-build/full gates, independently review, then fast-forward green master and close M3.
