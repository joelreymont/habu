---
title: Deflate a replaced zip member
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:51:06.157688+02:00"
---

Problem: ZIP:COMMIT writes every replaced member STORE (src/lib/zip.f COMMIT :389-395, 'replacements are STORE with fresh CRC and sizes'), so a package whose main part is replaced grows several times: Tender filling a Word form replaces word/document.xml and the filled copy is 344510 bytes against the blank's 80610 (document.xml 287136 stored, 23236 deflated in the blank); a filled workbook's sheet1.xml 37848 stored against 5514. The file opens, so nothing fails; a bidder uploads a form four times its size. Acceptance: a replaced member is written deflated when its source member was deflated, with correct sizes and CRC, untouched members still byte for byte. Files: src/lib/zip.f, src/lib/zip-test.f. Verify: replace a deflated member, commit, reopen: the member reads back equal and its compressed size is below its length. Depends: none. Ownership: src/lib/zip.f. Claim: unassigned. Reported by Tender's fill practice (stuart, 2026-11-10); not a Tender release requirement.
