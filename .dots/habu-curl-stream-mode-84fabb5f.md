---
title: CURL stream mode keeps the whole body in memory
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T11:09:21.944312+02:00"
---

From phil (Zen), 2026-11-05. Problem: in stream mode every delivered chunk stays allocated until the transfer ends: src/lib/net/curl.f:1195 STREAM-DELIVER appends each chunk, and the memory is freed only at :1781 STREAM-RESULT-TAKE, so a long stream (a model call, a large download read as chunks) grows to its whole size. Acceptance: a chunk the reader has taken is released (or its room reused) before the next arrives, so a stream's resident memory stays bounded by what is not yet taken; a test streams a body many times larger than one chunk and shows the peak held bytes bounded. Files: src/lib/net/curl.f, src/lib/net/curl-test.f. Verify: bin/hb --load src/lib/net/curl-test.f, model-test.f. Depends: none. Ownership: carl. Claim: unassigned.
