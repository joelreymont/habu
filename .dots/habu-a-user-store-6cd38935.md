---
title: "A user store moves a dynamic buffer's base"
status: open
priority: 1
issue-type: task
created-at: "2026-11-10T19:45:24.927219+02:00"
---

Problem: the public heads CWORK:MEM#base and <NAME>#base of a DYNAMIC-BUFFER take a user's store: 'FOOB CWORK:MEM#base !' makes '0 ZB' (a CWORK buffer) answer a typed pointer 59056 bytes past the 1-byte FOOB, and 'FOOB DB#base !' makes '3 DB' answer one 48 bytes past it. Run by the hunt of habu-user-src-arms-bc2ab900 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/storage-window-hunt.md, out of scope there); not rerun by the lead. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: checked source outside the definer's generated words cannot store into a buffer's base cell; the two programs are refused. Design in work with habu-user-src-arms-bc2ab900 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/Design-storage-window.md). Files: named by the design. Verify: the two programs, refused. Depends: the design. Ownership: the base cells of dynamic storage. Claim: unassigned.
