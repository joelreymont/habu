---
title: Make the LBUF-PEND absence rows fail without the seal undefines
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T02:37:12.659259+02:00"
---

Problem: test/layout-buffer.f:52-56 asserts the five LBUF-PEND words are undefined with `s" …" 0 search-wl 0= TTRUE`; `0 search-wl` (wordlist 0) answers 0 for every name, defined or not, so the tests pass on any engine and cannot catch a seal regression. Acceptance: the five tests fail on an engine built without the seal undefines and pass with them; the lookup used answers non-zero for a defined name in the same file, proven by one positive test beside them. Depends: none. Claim: unassigned.
