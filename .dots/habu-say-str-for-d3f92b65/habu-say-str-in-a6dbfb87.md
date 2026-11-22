---
title: Say str in ^u8 rows, src/arch/arm64/icode.f .. tools/source-discovery-test.f
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T16:34:07.192668+01:00"
---

Problem: 40 split-str files from src/arch/arm64/icode.f to tools/source-discovery-test.f still spell byte-string pairs ^u8 n / len / NUM:byte-len in rows (list /tmp/jerry-p1/files-a.txt). Acceptance: each such pair says str; destinations, regions and non-byte-strings are listed for the vector lanes; unmarked callers pass str; the engine builds and the files' suites pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p1a
