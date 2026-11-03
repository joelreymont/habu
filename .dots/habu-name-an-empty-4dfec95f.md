---
title: "Name an empty statement's failure in PG"
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T13:55:46.231833+02:00"
---

Problem (phil's review of lxytlvyt 'Carry PostgreSQL's whole report on failure', 2026-11-03): a statement with no command (a comment only) ends PGRES_EMPTY_QUERY and is classified failed (src/lib/pg.f CLASSIFY, around :1257-1261) with an empty SQLSTATE and an empty report, so PG:REFUSED$ and the failed spans are empty and a caller (Zen's MCP rejection text, Maki, Tender) shows nothing. src/lib/pg-test.f REPORT-CASES asserts that empty result today. Acceptance: the failure names its cause in the report span (libpq gives no text, so pg.f supplies one), or the empty statement gets an outcome of its own, chosen by the lane from pg.f's outcome contract; REPORT-CASES' second case asserts the new text; callers listed by phil (Zen test/worker.f, Maki test/project-service-case.f:389, test/chat-case.f:1608, :1614) checked. Files: src/lib/pg.f, src/lib/pg-test.f. Verify: HB_TMP=<dir> bin/hb --load test/db/pg-cluster.f. Depends: none. Ownership: unassigned. Claim: unassigned.
