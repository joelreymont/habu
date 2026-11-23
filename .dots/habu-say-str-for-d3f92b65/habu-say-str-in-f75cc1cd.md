---
title: Say str in ^u8 rows, src/lib/elf32.f .. src/lib/fs-anchored-test.f
status: closed
priority: 2
issue-type: task
created-at: "2026-11-22T16:34:07.244801+01:00"
closed-at: "2026-11-23T10:11:21.689078+01:00"
---

Problem: 15 split-str files from src/lib/elf32.f to src/lib/fs-anchored-test.f still spell byte-string pairs ^u8 n / len / NUM:byte-len in rows (list /tmp/jerry-p1/files-d.txt). Acceptance: each such pair says str; destinations, regions and non-byte-strings are listed for the vector lanes; unmarked callers pass str; the engine builds and the files' suites pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p1d
