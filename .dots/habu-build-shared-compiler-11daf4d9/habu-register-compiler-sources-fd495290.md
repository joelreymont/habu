---
title: Register compiler sources
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:54:38.127558+02:00\""
closed-at: "2026-08-15T14:07:28.872260+02:00"
close-reason: "Closed (vintage audit 2026-08-15, re-executed after the pool incident): sources (UTF-8 N/A recorded source.f:39). Production-consumed by the native chain; suites dual-registered, green through the real entry."
---

Full context: design sections 6.3 and 7.1 require module-local source identities, byte spans, origin chains, and stable source digests. Add source registry tables on the owned arena; context cache may deduplicate bytes but imports remap to local IDs. Acceptance: invalid ranges, foreign owners, bad UTF-8 assumptions where applicable, and origin cycles reject; equal bytes digest stably; frozen modules own all source rows. Dependency: compiler arena.

Claim: agent=ir-source workspace=.jj-ws/habu-register-compiler-sources-fd495290
