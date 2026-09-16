---
title: Merge short zero gaps in the captured DATA runs
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-16T13:08:16.432605+03:00\""
---

Problem: the captured DATA image stores non-zero runs with an 8-byte (offset u32, length u32) row each; the census tool tools/data-table-census.f (2026-09-16) measured 802,164 payload bytes in 258,216 runs, an average run of 3.1 bytes costing 11.1 bytes, so the run rows are 2,065,728 of the 2,867,892 DATA image bytes (72 percent). The worst owners are live cell arrays of small integers (the persisted USIGS signature pool plus NORET: 358,767 bytes in 200,996 runs; SYMS-BOOT: 198,117 bytes in 70,140 runs) where every cell becomes its own run. Acceptance: the run scanner in src/habu/aot-capture.f (ACAP-SCAN-RUNS, and the stripped path in src/habu/aot-lib.f which shares the format) merges runs separated by fewer than 8 zero bytes (a zero gap shorter than a row costs less to carry than to split), the restore stays a plain copy of the rows, the DATA image shrinks by the measured row overhead (report before/after from the census tool), byte fixpoint, stripped-* and snapshot suites green. Files: src/habu/aot-capture.f, src/habu/aot-lib.f, test/gate-aot-image.f if the copy-loop pin changes, tools/data-table-census.f for the measurement. Verify: census before/after, fixtures, fixpoint. Depends: habu-qualify-habu-for-9ccd0432 (optimization, after release). Ownership: hazel line. Claim: agent=hazel-table-fill workspace=.jj-ws/hazel-table-fill.
