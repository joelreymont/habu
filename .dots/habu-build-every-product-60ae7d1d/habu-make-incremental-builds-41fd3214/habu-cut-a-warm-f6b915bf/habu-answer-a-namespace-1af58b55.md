---
title: Answer a namespace miss without re-indexing
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T05:03:46.335943+02:00"
---

Problem: src/core/checker.f NS-FIND re-indexes every record on a miss; a warm engine build asks 183 times for wids no package holds (975 159 times, 608 23, 977 once), 313 us each (57 ms). The rebuild is the only guard against index rows NS-NOTE cannot see change: the namespace-private (src/habu/habu2.f NAMESPACE-PRIVATE :1057) and replay-private (REPLAY-PRIVATE :1248, :1378) stores into a record's wid cells; linker.f MOVE-IN (:5081) and the close (:5390) swapping the namespace marker, which hides packages made between the last lookup and the swap; record-wid! retiring and reviving a package row (checker.f :13123, :13197, :13289, :13528, XREF:RETIRE); and the record count falling and growing back to the same value between two lookups (replay-close, FORGET-SPACE linker.f:5405, a rollback's ndict!). Acceptance: a miss answers 0 without re-indexing, every lookup answers as a full re-index would after each of those changes, the warm build's time falls. Depends: none. Claim: unassigned; design first.
