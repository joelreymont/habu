---
title: Refuse at validation every object the link would refuse
status: active
priority: 2
issue-type: task
created-at: "2026-11-07T03:06:15.638838+02:00"
---

Problem (L4 Fable review F1, 2026-11-07, probes /tmp/carl-rev-l4f/sweep-count-flip.log and tools/sweep.sh): FRESH? admits object shapes the apply then refuses mid-link, so a bad object ends the build LINKER:E-FAILED after earlier entries were applied instead of compiling the file, and the object is never rewritten, so every later warm build fails the same way until the out root is deleted (design-fobj item 10: a valid object that fails to link is a defect). Three shapes: (1) src/habu/linker.f PKG-FLAG-CK / ROLE-CK admit a package entry whose own-private flag is 0 with a later role-4 entry; ROLE-WID throws E-COMP-STATE (flip at byte 226 of count.f's object, rc 67 -8570). (2) CALL-CK / ADDR-CK check a site's range and alignment, not the instruction there; IMPORT-CODE's RELOC-EXTERNAL or ARM64:CALL! throws -8621 E-NPUB-RELOC or -8506 E-A64EMIT-REACH. (3) REC-REF-CK bounds an own-file record index by RECS-N, so a reference to a record not yet made passes and RESOLVE throws E-COMP-STATE (by reading). Acceptance: each shape makes FRESH? answer false (the file compiles, the object is rewritten, the program equals a cold build's); fix: ROLE-CK ties role 4 to a package with a private wordlist (own flag or an earlier KIND-PRIVATE entry), CALL-CK/ADDR-CK check the site's instruction as PLACE-CK/CHAIN-SIZE do, an own-file record reference is bounded by the entry's own record index + 1. Files: src/habu/linker.f, test/file-objects.f. Depends: L4 landed. Ownership: carl (lead), worker lane. Claim: agent=worker workspace=.jj-ws.noindex/carl-f1 (tests brief first).
