---
title: Install retired records from file objects
status: active
priority: 2
issue-type: task
created-at: "2026-10-24T04:12:11.429031+03:00"
---

Problem: warm-diag.md R3: five files that undefine their own words (xref.f NAME-OUT, generated-declaration-protection.f SNAPSHOT, layout-buffer-address.f, prefix-boundary.f INSTALL, workspace.f CWORK:BUFFER; 211 defs) store no object: ROW-SYM answers false for a retired record, so FULL-ORD/FULL-REC? refuse origin-record symbol and EARLY-ROW no-symbol (dc:1168-1226). On the stack undefine re-runs live, so the old replay plan (wordlist cell -2, FILE-RETIRE hook) does not apply. Acceptance: a retired record of the closing file is the one match at its xt and keeps the journal's symbol; the five files reused; warm == cold. Files: tools/native-definition-cache.f. Verify: P0 on the lane, the lane's undefine case. Brief: .jj-ws.noindex/carl-dots/.out/carl-file/brief-fix-r3.md. Depends: none. Ownership: ROW-SYM, FULL-ORD, FULL-REC?, EARLY-ROW, FULL-ROW. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fr3
