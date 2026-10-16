---
title: Read aot-artifact-rows fields through LE
status: open
priority: 3
issue-type: task
created-at: "2026-10-16T08:34:57.392068+03:00"
---

Problem: test/aot-artifact-rows.f:127-139 defines private U32@, U32! and U64@ that duplicate LE:U32@, LE:U32! and LE:U64@ (src/lib/le.f:24-31): same byte order, same results. Acceptance: the three private words are deleted, their uses call the LE words, and the file requires src/lib/le.f; its row passes. Files: test/aot-artifact-rows.f. Verify: the aot-artifact-rows SUITE row. Depends: none. Ownership: test/aot-artifact-rows.f. Claim: unassigned.
