---
title: Make the LBUF-PEND absence rows fail without the seal undefines
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T02:37:12.659259+02:00"
---

Problem: test/layout-buffer.f:53-57 asserts the five LBUF-PEND words are undefined with `s" …" 0 search-wl 0= TTRUE`; `0 search-wl` (wordlist 0) answers 0 for every name, defined or not, so the rows pass on any engine and cannot catch a seal regression (phil 2026-10-30; evidence .jj-ws.noindex/phil-arena/.out/handoff-arena-ra2.md "Out of scope"). Acceptance: the five rows fail on an engine built without the seal undefines and pass with them; the lookup used answers non-zero for a defined name in the same file, proven by one positive row beside them. Files: test/layout-buffer.f. Verify: bin/hb --load test/layout-buffer.f rc 0 on the product engine; the rows red on an engine with the undefines removed. Depends: none. Ownership: test/layout-buffer.f. Claim: unassigned.
