---
title: "Count rows that name a word, and the word an alias binds, in the word's time"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:17:33.257880+02:00"
---

Problem: a word's time (linker.f WORD-TIME, carl-words :3059) is kept while its entry, emission and text ranges hold the old bytes, but a retirement row (FACT+, undefine in the same piece), a late KIND-PRIVATE entry naming a package record of the piece (LATE+) and HELD-UNDEFINE rows name the word from outside those ranges, and an alias or type entry binds another word's result without holding its bytes (RECORD+ :1165, TYPE-SOURCE :3398). Once dependents are judged by word time, undefine X at the end of X's piece or a changed alias source leaves the time unchanged and a dependent links stale. Found by the review of zrztmqlr (finding 1) and design 3.1. Acceptance: TIMES holds an eighth cell per word covering the bound word's time and every row or entry outside the three ranges that names the word; WORD-TIME keeps the old time only when it is equal. Files: src/habu/linker.f. Verify: cold/warm cmp; times printer shows the alias and the retired word get new times, their neighbours keep theirs. Depends: word-times fold-ins. Must land before FILES rows lose their time. Ownership: carl. Claim: unassigned.

Merged (carl, 2026-11-10) as rlmuzpok. Waiting case: a word that binds a record with no time (an alias of one of hb's own words) takes a new time at every compile of its piece (WORD-TIME), so its users compile with it; right, and more than needed. No engine or kernel object has an alias entry today (0 of kind 2).
