---
title: Parse every decimal float, correctly rounded
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T17:47:53.979171+01:00"
---

Problem: src/lib/float.f STR>FLOAT answers none for a plain decimal with more than 18 fraction digits or an integer part past i64 (FL-ENGINE-DECIMAL-ADMITTED?, kept to agree with the engine's num-parse, src/habu/prims.f:807, kernel-x64.f NUM-PARSE,), so JSON:READ-FLOAT (json/read.f:737) and pg-rows (pg-rows.f:353) reject valid numbers such as 0.1234567890123456789; and its header says it is not correctly rounded, so a value can be one ulp off. Acceptance: STR>FLOAT and the engine's float literals read every decimal of their grammar (exponent within i64) to the correctly rounded binary64, one algorithm for both; float-test checks the boundary cases (19+ fraction digits, halfway cases, subnormals). Depends: none. Claim: unassigned.
