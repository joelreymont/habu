---
title: Avoid cache-hit source rebuilds
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T04:49:18.303794+02:00\""
---

Full context: tools/build-fixpoint.f generates and writes both complete stage2/stdin source artifacts before stamp comparison, then regenerates them again on misses. Compute the canonical key from constituent bytes or reuse a content-addressed source artifact so hits perform no generated-source writes and misses emit once; add open/byte/time ratchets. Group with active build-fixpoint hardening dots.
