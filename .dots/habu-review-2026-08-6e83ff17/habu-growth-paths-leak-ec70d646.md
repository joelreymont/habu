---
title: growth paths leak the previous mapping
status: closed
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.927556+02:00"
closed-at: "2026-08-23T14:20:11.688766+02:00"
close-reason: implemented, reviewed, merged, gates green: JW-GROW, SELECT-BUF and SHAPE:ALLOC release the span they replace in allocate-install-release order (1c877c91, landed via merge 6ecb02fe); the structural witness lib/test/mapped.f LIVE? (write(2) from the old span to a real unlinked file - /dev/null never reads the buffer and would have passed a freed span) is proved both ways and scheduled; each site's test red on a mutant that deletes the release; consumer suites green.
---

Problem: lib/json-write.f:84-87 JW-GROW allocates, copies, installs and never releases the old span (every doubling leaks); same shape lib/build-cache.f:246-251 SELECT-BUF and lib/test/src-shape.f:42 ALLOC (one span per LOAD). lib/vector.f:373-379 and byte-buffer.f:791-797 show the correct copy-install-release order. Acceptance: each releases the old span after install; a test grows past two doublings and asserts the mapping count. Files: lib/json-write.f, lib/build-cache.f, lib/test/src-shape.f. Verify: the tests. Depends: none. Ownership: lib memory. Claim: closed (landed on master via 6ecb02fe).
