---
title: Bisect the clobber lookup once tables are big
status: open
priority: 2
issue-type: task
created-at: "2026-08-09T22:36:59.141247+02:00"
---

src/compiler/native/clobber.f ROW-OF is a linear scan; measured 7.4 ns/row, ~37 us worst-case at 5000 rows, ~3 s of total scanning for a 4000-routine cut population, ~20 s at 10000. The table is publication-ordered (publish.f SLOT-CK enforces strictly increasing claims), so bisection is available - but only if ROW+ itself REFUSES an out-of-order append (new named error code), else the order is a hope not a fact (today ORDER-CK only dies during reclaim). Do both: the order refusal in ROW+ and the bisecting ROW-OF/FLOOR-ROW, with a fixture proving the refusal and one proving lookup equivalence against the linear scan over a grown table. Files: src/compiler/native/clobber.f, test/compiler/native-clobber.f. Verify: native-clobber, native-publish suites. Depends: none (perf matters at the cut's population; correctness unaffected today).
