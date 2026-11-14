---
title: "Keep the first host's line in a PG refusal"
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T11:36:46.303733+02:00"
---

Every refused PG:CONNECT text lacks the first host's 'connection to server at "...", port N failed: ' line: one host gives '\nConnection refused\n\tIs the server...'; with 91 hosts, hosts 2-91 carry the line and host 1 does not (code review of kmpzlyvm). Inference, not measured: LIB-SET-NONBLOCKING (src/lib/pg.f:216), called right after PQconnectStart, clears libpq's error buffer. Acceptance: a refused connect's text names every host it tried, first included; regression row in src/lib/pg-test.f against test/db/pg-cluster.f. Related: habu-close-the-pg-27b3f4d3.
