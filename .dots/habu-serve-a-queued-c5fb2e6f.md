---
title: Serve a queued miss the lazy intake could not hold
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T07:38:05.793032+02:00"
---

Problem: src/core/checker.f ASIG-MISS+ (the lazy intake's queue of names one check pass could not resolve) drops a name once ASIG-MISS-CAP ($800 bytes) is used and says nothing. CK-AOT-RETRY-DUE? decides a retry from the names that fit: a body whose first 2 KB of misses no seeded row answers, followed by a name a seeded row does answer, gets E-UNDEFINED instead of a retry, so the cap costs an answer, not only the pass the header comment promises. A fixed limit on checked input is also against the no-cap rule. Found by TOP-JUDGE worker 7 while reading TOP-TAKE-ROW?. Acceptance: no miss is dropped (the queue grows with the pass, in the checker's own storage) or the retry decision sees every miss; a test with more misses than the old cap holds, the servable one last, checks and resolves; existing seeded-row tests unchanged. Files: src/core/checker.f (ASIG-MISS-CAP, ASIG-MISS, ASIG-MISS+, ASIG-MISS-LEN@, ASIG-MISS-A@, CK-AOT-MISSES, CK-AOT-MISS-SERVED?), a test under test/. Verify: the new test, then the full suite (shared checker behaviour). Depends: none. Ownership: the words named. Claim: unassigned.
