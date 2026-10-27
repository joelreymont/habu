---
title: Place CELL and ALLOC right in x64 record flags
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T11:07:35.448168+02:00"
---

src/habu/link-x64.f:403-407 FLAGS shifts the whole compact kind byte to bit 50, but the capture packs DKIND in kind bits 0-1, DNAME-CELL in bit 2 and DNAME-ALLOC in bit 3 (src/habu/aot-capture.f:1439-1441). On x64 CELL and ALLOC therefore land at record bits 52-53, inside DNAME-MIN-IN (52-59), instead of 48 and 47 (src/habu/layout.f:298-319). Found by the C2 redo worker (carl-c2), whose FLAGS edit keeps the misplacement. Fix after C2 lands: decode each field to its own bit; prove with an x64 product whose variable and create records read back CELL/ALLOC and the right min-in. Lane: carl.
