---
title: "XREF:SEALED? on a null record crashes hb"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T17:51:49.534872+02:00"
---

Problem: src/habu/xref.f:509 SEALED? ( ptr n -- bool ) reads the flags of the record it is given; called with XREF-NULL from checked source hb dies with SIGSEGV, rc 139 (census run, /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/describe-census.md item 4). Checked source must not be able to crash the engine through a public word. Not yet known: which other public XREF readers that take a record do the same on XREF-NULL, and whether the type should keep a null record out or each reader should refuse. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: the reproducer, every public reader's answer on XREF-NULL, the cause, and where one guard belongs; note habu-split-dict-out-e6fc8a93 moves these readers to DICT. Acceptance: no public XREF or DICT word given XREF-NULL ends hb by a signal; each answers or throws a named code. Files: src/habu/xref.f. Verify: the reproducer in tools/xref-test-fixture.f. Depends: none. Ownership: the record readers' null case. Claim: agent=bob workspace=.jj-ws.noindex/habu-xref-sealed-on-b27a8e57

Hunt 2026-11-10 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/null-hunt.md): XREF-NULL is address 0 and all 22 crashing readers read through XREF-CELL@. Settled by the long-term rule: one guard in XREF-CELL@ that throws a new named code E-XREF-NULL; hide.f's readers by index and the callers that pass an untested find are left. Fix in work (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/Guard-the-record-readers-against-null.md).

Review 2026-11-10 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/null-review.md): REVISE: CK-REC-WID and CHECKER-RESOLVE:CK-REC-NAME$ in checker.f read records with no null test and still crash. Fold in work (briefs8/Fold-review-into-null-guard.md): one guarded reader and E-XREF-NULL (7231) in src/core/util.f, read by xref.f and checker.f.
