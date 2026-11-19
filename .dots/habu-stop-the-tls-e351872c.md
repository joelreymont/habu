---
title: Stop the TLS test peer with its test
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T15:11:41.349620+02:00"
---

Problem: src/lib/net/test-tls-peer.f starts openssl s_server as the leader of its own process group, and only the test's finally stops it; a test ended by a signal or a crash leaves s_server listening, reparented to init, and its scratch directory holding the keys (seen after a SIGTERM and a SIGSEGV). The suite's pool walks the process tree, so only a pool timeout is covered; test/db/pg-cluster.f states the same limit. Acceptance: a test killed by SIGKILL while serving leaves no s_server running and no scratch directory with its keys. Depends: none. Claim: unassigned.
