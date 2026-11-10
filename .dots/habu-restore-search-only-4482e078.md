---
title: "Restore search-only's mode when its run throws"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:14:42.774825+02:00"
---

Problem: test/source-root-exe-test.f CD-SEARCH-ONLY (:554-560) sets its directory to mode 0111 and restores it after RUN-TREE without finally; when RUN-TREE throws, search-only stays unreadable, the at-exit FS REMOVE-TREE (src/lib/fs.f:686 opens each directory O_RDONLY) fails on it and the scratch tree leaks, which test/gate-pool.f reports as scratch not removed on a test already failing. CD-LOCKED beside it (:547-552) restores under finally. Found by the review of 'Say only what ROOT! knows of a failed root' (2026-11-10). Acceptance: CD-SEARCH-ONLY restores the mode however RUN-TREE ends, as CD-LOCKED does. Files: test/source-root-exe-test.f. Verify: hb --load test/source-root-exe-test.f exits 0; by reading, the restore is under finally. Depends: none. Ownership: test/source-root-exe-test.f. Claim: unassigned.
