---
title: "Check an object's own sum before linking it"
status: open
priority: 3
issue-type: task
created-at: "2026-11-07T03:06:15.699704+02:00"
---

Problem (L4 Fable review F2, 2026-11-07, /tmp/carl-rev-l4f/sweep-main-flip.log): an object carries no checksum over its own bytes, so a flipped content byte links silently: of 1010 single-byte flips in main.f's object, 46 linked into a different program, 132 into the same, and 7 died rc 74 at LINKER:WRITE ('a product routine names a word outside the product', a die a body cannot catch). FRESH? checks structure only (DECISIONS 2026-11-07 'linking a file's object'). BUILT+ already hashes the whole object read (OBJECT-SUM), so a sum stored in the object costs no extra read. Acceptance (if taken): a flipped byte anywhere makes the file compile. Decide with L5's freshness design: this changes the saved object format (hard cutover). Files: src/habu/linker.f. Ownership: carl (lead). Claim: unassigned.
