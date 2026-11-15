---
title: STR>FLOAT refuses 19+ digits, misrounds
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T23:35:03.557527+02:00"
---

Problem: STR>FLOAT (src/lib/float.f:175), and through it JSON:READ-FLOAT (src/lib/json/read.f:712) and PG rows (src/lib/pg-rows.f:375), answer none for a decimal with 19 or more fraction digits, and do not round correctly to binary64. Probe from Maki (2026-11-07, habu master 0a4d9600): s" 0.0017453292519943296" STR>FLOAT -> none (18 digits parse); s" 0.1000000000000000055511151231257827" -> none; s" 1.7976931348623157e308" -> bits 9218868437227405308, correctly rounded is 9218868437227405311 (3 ulp off). Found by tim while adding JSON:VALUE=, which now compares numbers on lexemes and does not depend on this. Acceptance: any decimal JSON admits parses to the correctly rounded binary64 (round half to even), whatever its digit count; the three cases above as tests. Files: src/lib/float.f, src/lib/float-test.f. Verify: hb --load src/lib/float-test.f; hb --load src/lib/json-read-test.f. Depends: none. Ownership: float.f. Claim: unassigned.
