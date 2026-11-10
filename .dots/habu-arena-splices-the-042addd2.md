---
title: ARENA splices the package name into generated text unescaped
status: open
priority: 1
issue-type: task
created-at: "2026-11-10T19:45:24.910830+02:00"
---

Problem: src/core/arena.f SOURCE writes s" <package>:<name>" into the generated NEW word with the package name (CHECKER-AUTH-PACKAGE$) as it stands. 'package Z public : QZ" ( n ptr u8 n -- n ptr u8 n ) {: w:n a:ptr u:n :} 1 a u ; ;package' then 'package P"Z public ARENA QZ pr2 ...' makes QZ-NEW hand out ptr pr2 records 1 byte apart where a plain package gives 16: rc 0 under --load and under tools/check.f. A quote in the arena's own name is refused by the STRUCTURE tail rule. Run by the hunt of habu-user-src-arms-bc2ab900 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/storage-window-hunt.md, door C); not rerun by the lead. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: no byte a caller chose reaches generated text except as a name or type the definer validated; the program is refused. Design in work with habu-user-src-arms-bc2ab900 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/Design-storage-window.md). Files: src/core/arena.f, a case in test/arena.f. Verify: the program, refused. Depends: the design. Ownership: ARENA's generated text. Claim: unassigned.
