---
title: Find the intermittent stack overflow in whitebox-engine-key
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T22:28:28.908168+03:00"
---

Full-suite run on master+diag (carl-dgate.Xelh, 2026-10-23): whitebox-engine-key asserts 5 6, unsealed engine build died 'hb: stack bounds exceeded (machine)', rc 102. Passed alone and 4x concurrent on the same engine. Old piece-1 gate also saw whitebox-engine-key rc 67. Logs were cleaned by the suite; next failure: keep the pool err log.
