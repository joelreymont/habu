---
title: Write secrets on Linux
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T20:22:45.398148+02:00"
---

Problem: SECRET:WRITE throws on Linux, and src/lib/secret/linux.f searches only attributes named service and account, so a program cannot store its credential where secret-tool would, nor find one stored under other keys (omabox docs/habu-rewrite.md, Habu libraries). Acceptance: a program binds the attribute keys and label once; SECRET:WRITE stores as secret-tool store does (replacing the item with the same attributes) and SECRET:READ finds an item secret-tool stored under those keys; its test. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/habu-secret-write.
