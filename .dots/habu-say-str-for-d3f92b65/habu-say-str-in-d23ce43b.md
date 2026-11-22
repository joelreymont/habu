---
title: Say str in ^u8 rows, src/lib/aio-test.f .. src/lib/diff-test.f
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T16:34:07.226234+01:00"
---

Problem: 27 split-str files from src/lib/aio-test.f to src/lib/diff-test.f still spell byte-string pairs ^u8 n / len / NUM:byte-len in rows (list /tmp/jerry-p1/files-c.txt). Acceptance: each such pair says str; destinations, regions and non-byte-strings are listed for the vector lanes; unmarked callers pass str; the engine builds and the files' suites pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p1c
