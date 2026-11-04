---
title: Name what triggers each file cache refusal
status: closed
priority: 2
issue-type: task
created-at: "2026-10-26T12:51:54.661178+02:00"
closed-at: "2026-10-27T05:44:04.194348+02:00"
close-reason: "landed on master 9f905e31: full suite 503/503 (477 s), two-generation gen2..5 byte-equal, landing review ACCEPT (census lines landed; ranking fed aa3c25a5)"
---

Engine cold build (ulxuqysn) stores 33 of 194 files. 126 frames are ordered=1: 83 reason=mapped-compile-time-read word=ROW-LENS (the require registry's mapped column, src/core/include.f:36, reached through an executed word's closure in CAPTURE-RUN/REACH-DATA), about 28 run-host-unchecked through typed storage definers (PTR-VARIABLE, PERSISTED-PTR-VARIABLE, RESERVED-PTR-U8-CELL...). 39 files refuse tier and 32 earlier-write-before-load with no trigger named. Lane: diagnostics-only lines ordered-run (reason/holder/via/run), earlier-write (site owner), tier (value), then a cold census. The ranking feeds habu-refuse-unrecordable-compile-aa3c25a5. Workspace: carl-census, brief ~/.cache/tmp/carl-file/brief-trigger.md.

Census (trtvwuul engine, cold, ~/.cache/tmp/carl-cen.hpML): 33 of 194 stored. Refused: ordered-dependency 59, tier 39, earlier-write 32, run-host-unchecked 11, run-host-rebind 10, other 10. Ordered runs: 83 files hold ROW-LENS (REQUIRE-REG) through run=require (78) or required (5), reached by src/core/include.f ROW-LEN-ADDR via ROW-LEN@. Earlier-write first sites: RESOLUTION-SOURCE-XT+0 71, PTRA-BOOT+8 37, CHECKER-COLON-N+0 5 (lanes 35c0472f, 23fa6d58, a50978c5). Tier value=0: 39 files (src/arch/arm64/*, tools/native-emit*.f). Unchecked runs through typed definers: PTR-VARIABLE 5, PERSISTED-PTR-VARIABLE 5, RESERVED-PTR-U8-CELL 4, TRUST 2, REG-PROTECT 2, PTR-U8-TABLE 2, ENUM/TDECL-UNCHECKED$ 2. Close once trtvwuul lands.
