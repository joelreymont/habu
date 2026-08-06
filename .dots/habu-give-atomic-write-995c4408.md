---
title: Give ATOMIC-WRITE-FILE a unique temp
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T17:55:21.723893+02:00"
---

lib/fs-mutate.f:202-205 ATOMIC-WRITE-FILE builds its temp as path + a FIXED 4-byte '.tmp' suffix (FS-MUT-ATOMIC-SUFFIX, line 36), then renames. Two processes writing the same destination therefore share one temp file: writer B truncates and rewrites it while writer A is mid-write, then A renames that partially-written temp into place. The rename is atomic, the write is not, so a truncated file lands at the destination while both callers believe they published a whole one. This is reachable today: lib/object-cache.f:112 and lib/object-index.f:115 write the machine-global build cache at PERSIST$/hb-build-cache (~/.cache/habu-gate, 7477 entries), shared by every workspace on the host, and identical content across lanes means identical keys means the same temp path. The tree already has the correct pattern twice -- lib/content-key.f CK-CACHE-UNIQUE-TMP (mono-ns stamp plus existence check plus retry) and lib/fs-mutate.f MAKE-TEMP-DIR (FS-MUT-BUILD-TEMP-TRY, seed plus attempt, FS-MUT-TMP-RETRIES). Route ATOMIC-WRITE-FILE through that same uniqueness, and cover it with a test that has two processes publish concurrently to one path. Found while attributing habu-attr-the-candidate-4a2356c5; not proven to be the cause of that red.
