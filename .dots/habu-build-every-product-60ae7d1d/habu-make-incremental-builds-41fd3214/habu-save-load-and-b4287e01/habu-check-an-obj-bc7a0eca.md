---
title: "Check an object's own sum before linking it"
status: closed
priority: 3
issue-type: task
created-at: "2026-11-07T03:06:15.699704+02:00"
closed-at: "2026-11-07T04:41:40.200647+02:00"
close-reason: "Landed with L5: the header carries a SHA-256 over the object past it, checked first in HEADER-CK (CK-SUM); a mismatch compiles the file."
---

Problem (L4 Fable review F2, 2026-11-07, /tmp/carl-rev-l4f/sweep-main-flip.log): an object carries no checksum over its own bytes, so a flipped content byte links silently: of 1010 single-byte flips in main.f's object, 46 linked into a different program, 132 into the same, and 7 died rc 74 at LINKER:WRITE ('a product routine names a word outside the product', a die a body cannot catch). FRESH? checks structure only (the lead's ruling, 2026-11-07). BUILT+ already hashes the whole object read (OBJECT-SUM), so a sum stored in the object costs no extra read. Acceptance: a flipped byte anywhere makes the file compile. Folded into L5-I1 (lead's L5 decision 2026-11-07): a SHA-256 over the object's bytes after the header field, checked first in HEADER-CK; hard cutover of the object format. Files: src/habu/linker.f. Ownership: carl (lead). Claim: agent=worker, L5-I1 lane (dot habu-file-objects-l5-c2dba0e3).
