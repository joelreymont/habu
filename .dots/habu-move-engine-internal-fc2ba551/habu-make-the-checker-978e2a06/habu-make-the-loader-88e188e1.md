---
title: Make the loader and xref internals private
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T16:11:36.433984+01:00"
---

Problem: CHECKER-RESOLVE:CK-REC-NAME$ and CK-REC-WID, CAST-ERROR:RAISE, GENERATED-DECL-NAME-PREFLIGHT:DICTIONARY-RECORDS, XREF-REQUIRE-INDEX (src/habu/xref.f), BOOT:TYPES-WL, and REQUIRE-SNAPSHOT and REQUIRE-RESTORE (src/core/include.f; tools/source-discovery.f :1423 :1429 calls them) are public or global. Acceptance: each is E-UNDEFINED outside its package, or the program that needs it is named. Depends: none. Claim: unassigned.
