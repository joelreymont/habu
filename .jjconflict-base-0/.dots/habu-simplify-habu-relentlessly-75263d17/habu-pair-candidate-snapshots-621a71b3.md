---
title: Pair candidate snapshots with the existing host
status: active
priority: 1
issue-type: task
created-at: "\"2026-08-23T21:07:08.958927+02:00\""
---

Real full-gate failure: test/snapshot-writer.f receives HABU_UNDER_TEST=<candidate product>, then source-loading snapshot builds fail because the product already provides the chain. Reuse the existing build convention <engine>-host: promote the already-built candidate sibling host and derive that path in snapshot-writer. No new env var, host rebuild, registry, fallback, or policy. Prove the real candidate snapshot suite and fixpoint gate.
